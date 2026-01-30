//! Loop trampoline elimination pass.
//!
//! SSA codegen creates "trampoline" blocks for loop back-edges to resolve phi
//! nodes. Instead of updating loop variables in-place, it creates new SSA values
//! in fresh registers and uses a separate block to shuffle them back:
//!
//!   .LOOP:
//!       ; ... loop body using %r9, %r10, %r11 ...
//!       movq %r9, %r14           ; copy old dest to new reg
//!       addq $320, %r14          ; modify new dest
//!       movq %r10, %r15          ; copy old frac to new reg
//!       addl %r8d, %r15d         ; modify new frac
//!       ; ... loop condition ...
//!       jne .TRAMPOLINE
//!   .TRAMPOLINE:
//!       movq %r14, %r9           ; shuffle new dest back
//!       movq %r15, %r10          ; shuffle new frac back
//!       jmp .LOOP
//!
//! This pass detects trampoline blocks and coalesces the register copies:
//!   1. For each trampoline copy %src -> %dst, find where %src was created in
//!      the predecessor (as a copy from %dst followed by modifications).
//!   2. Rewrite those modifications to target %dst directly.
//!   3. NOP the initial copy and the trampoline copy.
//!   4. Redirect the branch directly to the loop header.

use super::super::types::*;
use super::helpers::*;

pub(super) fn eliminate_loop_trampolines(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = store.len();
    if len < 4 {
        return false;
    }

    let mut changed = false;

    // Build a map of label_name -> line_index for all labels.
    let mut label_positions: Vec<(u32, usize)> = Vec::new();
    let mut label_branch_count: Vec<u32> = Vec::new();
    let mut max_label_num: u32 = 0;

    for i in 0..len {
        if infos[i].is_nop() { continue; }
        if infos[i].kind == LineKind::Label {
            let trimmed = infos[i].trimmed(store.get(i));
            if let Some(n) = parse_label_number(trimmed) {
                label_positions.push((n, i));
                if n > max_label_num { max_label_num = n; }
            }
        }
    }

    if label_positions.is_empty() {
        return false;
    }

    // Build label_num -> line_index lookup
    let mut label_line: Vec<usize> = vec![usize::MAX; (max_label_num + 1) as usize];
    for &(num, idx) in &label_positions {
        label_line[num as usize] = idx;
    }

    // Count branch references to each label
    label_branch_count.resize((max_label_num + 1) as usize, 0);
    for i in 0..len {
        if infos[i].is_nop() { continue; }
        match infos[i].kind {
            LineKind::Jmp | LineKind::CondJmp => {
                let trimmed = infos[i].trimmed(store.get(i));
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        if (n as usize) < label_branch_count.len() {
                            label_branch_count[n as usize] += 1;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Find trampoline blocks
    for &(tramp_num, tramp_label_idx) in &label_positions {
        if label_branch_count[tramp_num as usize] != 1 {
            continue;
        }

        // Parse the trampoline block contents
        let mut tramp_moves: Vec<(RegId, RegId)> = Vec::new();
        let mut tramp_jmp_target: Option<u32> = None;
        let mut has_stack_load = false;
        let mut tramp_stack_loads: Vec<(i32, RegId, usize, usize)> = Vec::new();
        let mut tramp_all_lines: Vec<usize> = Vec::new();

        let mut j = tramp_label_idx + 1;
        while j < len {
            if infos[j].is_nop() || infos[j].kind == LineKind::Empty {
                j += 1;
                continue;
            }
            let trimmed = infos[j].trimmed(store.get(j));

            // Check for movq %regA, %regB
            if let Some(rest) = trimmed.strip_prefix("movq %") {
                if let Some((src_str, dst_str)) = rest.split_once(", %") {
                    let src_name = format!("%{}", src_str);
                    let dst_name = format!("%{}", dst_str.trim());
                    let src_fam = register_family_fast(&src_name);
                    let dst_fam = register_family_fast(&dst_name);
                    if src_fam != REG_NONE && dst_fam != REG_NONE && src_fam != dst_fam {
                        tramp_moves.push((src_fam, dst_fam));
                        tramp_all_lines.push(j);
                        j += 1;
                        continue;
                    }
                }
            }

            // Check for movslq %regA, %regB
            if let Some(rest) = trimmed.strip_prefix("movslq %") {
                if let Some((src_str, dst_str)) = rest.split_once(", %") {
                    let src_name = format!("%{}", src_str);
                    let dst_name = format!("%{}", dst_str.trim());
                    let src_fam = register_family_fast(&src_name);
                    let dst_fam = register_family_fast(&dst_name);
                    if src_fam != REG_NONE && dst_fam != REG_NONE && src_fam != dst_fam {
                        tramp_moves.push((src_fam, dst_fam));
                        tramp_all_lines.push(j);
                        j += 1;
                        continue;
                    }
                }
            }

            // Check for stack load pattern: movq -N(%rbp), %rax
            if let LineKind::LoadRbp { reg: 0, offset, .. } = infos[j].kind {
                let mut k = j + 1;
                while k < len && (infos[k].is_nop() || infos[k].kind == LineKind::Empty) {
                    k += 1;
                }
                if k < len {
                    let next_trimmed = infos[k].trimmed(store.get(k));
                    if let Some(rest) = next_trimmed.strip_prefix("movq %rax, %") {
                        let dst_name = format!("%{}", rest.trim());
                        let dst_fam = register_family_fast(&dst_name);
                        if dst_fam != REG_NONE && dst_fam != 0 {
                            has_stack_load = true;
                            tramp_stack_loads.push((offset, dst_fam, j, k));
                            tramp_all_lines.push(j);
                            tramp_all_lines.push(k);
                            j = k + 1;
                            continue;
                        }
                    }
                }
                break;
            }

            // Check for jmp .LBB_N (final instruction)
            if infos[j].kind == LineKind::Jmp {
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        tramp_jmp_target = Some(n);
                        tramp_all_lines.push(j);
                    }
                }
                break;
            }

            break;
        }

        let target_num = match tramp_jmp_target {
            Some(n) => n,
            None => continue,
        };

        if tramp_moves.is_empty() && !has_stack_load {
            continue;
        }

        // Find the conditional branch that targets this trampoline
        let mut branch_idx = None;
        for i in 0..len {
            if infos[i].is_nop() { continue; }
            if infos[i].kind == LineKind::CondJmp {
                let trimmed = infos[i].trimmed(store.get(i));
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        if n == tramp_num {
                            branch_idx = Some(i);
                            break;
                        }
                    }
                }
            }
        }

        let branch_idx = match branch_idx {
            Some(i) => i,
            None => continue,
        };

        // Per-move coalescing
        let mut move_coalesced: Vec<bool> = Vec::with_capacity(tramp_moves.len());
        let mut coalesce_actions: Vec<(usize, RegId, RegId)> = Vec::new();
        let mut copy_nop_lines: Vec<usize> = Vec::new();
        for &(src_fam, dst_fam) in &tramp_moves {
            let src_64 = REG_NAMES[0][src_fam as usize];
            let dst_64 = REG_NAMES[0][dst_fam as usize];

            let mut copy_idx = None;
            let mut modifications: Vec<usize> = Vec::new();
            let mut scan_ok = true;

            let mut k = branch_idx;
            while k > 0 {
                k -= 1;
                if infos[k].is_nop() || infos[k].kind == LineKind::Empty {
                    continue;
                }
                if infos[k].kind == LineKind::Label {
                    break;
                }
                if matches!(infos[k].kind, LineKind::Call | LineKind::Jmp |
                    LineKind::JmpIndirect | LineKind::Ret) {
                    break;
                }

                let trimmed = infos[k].trimmed(store.get(k));

                let modifies_src = match infos[k].kind {
                    LineKind::Other { dest_reg } => dest_reg == src_fam,
                    LineKind::StoreRbp { .. } => false,
                    LineKind::LoadRbp { reg, .. } => reg == src_fam,
                    LineKind::SetCC { reg } => reg == src_fam,
                    LineKind::Pop { reg } => reg == src_fam,
                    _ => false,
                };

                if modifies_src {
                    let expected_copy = format!("movq {}, {}", dst_64, src_64);
                    if trimmed == expected_copy {
                        copy_idx = Some(k);
                        break;
                    }
                    let dst_32 = REG_NAMES[1][dst_fam as usize];
                    let expected_movslq = format!("movslq {}, {}", dst_32, src_64);
                    if trimmed == expected_movslq {
                        scan_ok = false;
                        break;
                    }
                    modifications.push(k);
                    continue;
                }

                if infos[k].reg_refs & (1u16 << src_fam) != 0 {
                    if infos[k].reg_refs & (1u16 << dst_fam) != 0 {
                        scan_ok = false;
                        break;
                    }
                    modifications.push(k);
                    continue;
                }

                if infos[k].reg_refs & (1u16 << dst_fam) != 0 {
                    scan_ok = false;
                    break;
                }
            }

            if !scan_ok || copy_idx.is_none() {
                move_coalesced.push(false);
                continue;
            }

            let copy_idx = copy_idx.unwrap();

            // Verify fall-through safety
            let check_regs = [dst_fam, src_fam];
            let mut fall_through_safe = true;
            let mut m = branch_idx + 1;
            let mut killed = [false; 2];
            let mut jumps_followed = 0u32;
            'ft_scan: while m < len {
                if infos[m].is_nop() || infos[m].kind == LineKind::Empty
                    || infos[m].kind == LineKind::Label {
                    m += 1;
                    continue;
                }
                if infos[m].kind == LineKind::Jmp {
                    if jumps_followed < 2 && (!killed[0] || !killed[1]) {
                        let trimmed = infos[m].trimmed(store.get(m));
                        if let Some(target) = extract_jump_target(trimmed) {
                            if let Some(n) = parse_dotl_number(target) {
                                if (n as usize) < label_line.len()
                                    && label_line[n as usize] != usize::MAX
                                {
                                    m = label_line[n as usize] + 1;
                                    jumps_followed += 1;
                                    continue 'ft_scan;
                                }
                            }
                        }
                    }
                    break;
                }
                if matches!(infos[m].kind, LineKind::JmpIndirect | LineKind::Ret) {
                    break;
                }
                if infos[m].kind == LineKind::CondJmp {
                    fall_through_safe = false;
                    break;
                }
                for i in 0..2 {
                    if killed[i] {
                        continue;
                    }
                    let reg = check_regs[i];
                    if infos[m].reg_refs & (1u16 << reg) != 0 {
                        fall_through_safe = false;
                        break;
                    }
                    let writes_reg = match infos[m].kind {
                        LineKind::Other { dest_reg } => dest_reg == reg,
                        LineKind::LoadRbp { reg: r, .. } => r == reg,
                        LineKind::SetCC { reg: r } => r == reg,
                        LineKind::Pop { reg: r } => r == reg,
                        _ => false,
                    };
                    if writes_reg {
                        killed[i] = true;
                    }
                }
                if !fall_through_safe {
                    break;
                }
                if killed[0] && killed[1] {
                    break;
                }
                m += 1;
            }
            if !fall_through_safe {
                move_coalesced.push(false);
                continue;
            }

            copy_nop_lines.push(copy_idx);

            for &mod_idx in &modifications {
                coalesce_actions.push((mod_idx, src_fam, dst_fam));
            }

            move_coalesced.push(true);
        }

        // Stack-load coalescing is not attempted (see comment in original code).
        let stack_coalesced: Vec<bool> = vec![false; tramp_stack_loads.len()];
        let stack_nop_lines: Vec<usize> = Vec::new();
        let stack_store_rewrites: Vec<(usize, String)> = Vec::new();

        let num_moves_coalesced = move_coalesced.iter().filter(|&&c| c).count();
        let num_stack_coalesced = stack_coalesced.iter().filter(|&&c| c).count();
        let total_coalesced = num_moves_coalesced + num_stack_coalesced;

        if total_coalesced == 0 {
            continue;
        }

        let all_coalesced = num_moves_coalesced == tramp_moves.len()
            && num_stack_coalesced == tramp_stack_loads.len();

        // Apply the register-register coalescing actions
        for &nop_idx in &copy_nop_lines {
            mark_nop(&mut infos[nop_idx]);
        }

        for &(mod_idx, old_fam, new_fam) in &coalesce_actions {
            let old_line = infos[mod_idx].trimmed(store.get(mod_idx)).to_string();
            if let Some(new_line) = rewrite_instruction_register(&old_line, old_fam, new_fam) {
                replace_line(store, &mut infos[mod_idx], mod_idx, format!("    {}", new_line));
            }
        }

        for &(store_idx, ref new_line) in &stack_store_rewrites {
            replace_line(store, &mut infos[store_idx], store_idx, new_line.clone());
        }
        for &nop_idx in &stack_nop_lines {
            mark_nop(&mut infos[nop_idx]);
        }

        if all_coalesced {
            for &line_idx in &tramp_all_lines {
                mark_nop(&mut infos[line_idx]);
            }
            mark_nop(&mut infos[tramp_label_idx]);

            let branch_trimmed = infos[branch_idx].trimmed(store.get(branch_idx)).to_string();
            if let Some(space_pos) = branch_trimmed.find(' ') {
                let cc = &branch_trimmed[..space_pos];
                let target_label = format!(".LBB{}", target_num);
                let new_branch = format!("    {} {}", cc, target_label);
                replace_line(store, &mut infos[branch_idx], branch_idx, new_branch);
            }
        } else {
            for (idx, &(src_fam, dst_fam)) in tramp_moves.iter().enumerate() {
                if !move_coalesced[idx] { continue; }
                for &line_idx in &tramp_all_lines {
                    if infos[line_idx].is_nop() { continue; }
                    let trimmed = infos[line_idx].trimmed(store.get(line_idx));
                    let src_64 = REG_NAMES[0][src_fam as usize];
                    let dst_64 = REG_NAMES[0][dst_fam as usize];
                    let expected = format!("movq {}, {}", src_64, dst_64);
                    if trimmed == expected {
                        mark_nop(&mut infos[line_idx]);
                        break;
                    }
                    let src_32 = REG_NAMES[1][src_fam as usize];
                    let expected2 = format!("movslq {}, {}", src_32, dst_64);
                    if trimmed == expected2 {
                        mark_nop(&mut infos[line_idx]);
                        break;
                    }
                }
            }
        }

        changed = true;
    }

    changed
}

/// Rewrite an instruction to use a different register family.
fn rewrite_instruction_register(inst: &str, old_fam: RegId, new_fam: RegId) -> Option<String> {
    let result = replace_reg_family(inst, old_fam, new_fam);
    if result == inst {
        None
    } else {
        Some(result)
    }
}
