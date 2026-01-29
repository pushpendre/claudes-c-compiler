#!/usr/bin/env python3
"""
Verify that a compiler can build the Linux kernel and boot it in QEMU.

Downloads kernel source, builds with specified compiler, boots in QEMU,
and verifies the custom printk message appears in dmesg.

Supports x86_64, ARM64, and RISC-V architectures.
"""

import argparse
import os
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path

KERNEL_VERSION = "6.9"
KERNEL_URL = f"https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-{KERNEL_VERSION}.tar.xz"
RAMFS_DIR = "/deps/ramfs"
DEPS_DIR = "/deps"
MAX_JOBS = 64

ARCH_TOOL_PREFIX = {
    'arm': 'aarch64-linux-gnu-',
    'riscv': 'riscv64-linux-gnu-',
}


def detect_arch_from_compiler(compiler):
    """Detect target arch from compiler name (e.g., ccc-arm -> arm, aarch64-linux-gnu-gcc -> arm)."""
    basename = os.path.basename(compiler)
    if 'aarch64' in basename or basename.endswith('-arm'):
        return 'arm'
    if 'riscv64' in basename or basename.endswith('-riscv'):
        return 'riscv'
    return None

# Architecture configurations
# (make_arch, cross_compile_prefix, kernel_image_path, qemu_cmd, qemu_args, console, busybox_name)
ARCH_CONFIG = {
    'x86': (
        None,  # native, no ARCH= needed
        None,  # no CROSS_COMPILE
        'arch/x86/boot/bzImage',
        'qemu-system-x86_64',
        ['-m', '512'],
        'ttyS0',
        'busybox-x86_64',
    ),
    'arm': (
        'arm64',
        'aarch64-linux-gnu-',
        'arch/arm64/boot/Image',
        'qemu-system-aarch64',
        ['-M', 'virt', '-cpu', 'cortex-a57', '-m', '512'],
        'ttyAMA0',
        'busybox-armv8l',
    ),
    'riscv': (
        'riscv',
        'riscv64-linux-gnu-',
        'arch/riscv/boot/Image',
        'qemu-system-riscv64',
        ['-M', 'virt', '-m', '512'],
        'ttyS0',
        'busybox-riscv64',
    ),
}


def get_expect_script(qemu_cmd, qemu_args, kernel, initrd, console):
    """Generate expect script for the given architecture."""
    qemu_full = f"{qemu_cmd} {' '.join(qemu_args)} -kernel {kernel} -initrd {initrd} -append \"console={console}\" -nographic"
    return f"""#!/usr/bin/expect -f
set timeout 180

spawn {qemu_full}

expect {{
    "localhost login: " {{
        send "root\\r"
    }}
    timeout {{
        puts "TIMEOUT waiting for login"
        exit 1
    }}
}}

expect {{
    "# " {{
        send "dmesg | grep 'custom kernel' | sed 's/is a/is a verified/'\\r"
    }}
    timeout {{
        puts "TIMEOUT waiting for shell"
        exit 1
    }}
}}

expect {{
    "is a verified custom kernel" {{
        puts "SUCCESS: Found verified custom kernel message"
    }}
    timeout {{
        puts "TIMEOUT waiting for dmesg output"
        exit 1
    }}
}}

expect "# "
send "poweroff -f\\r"

# Don't wait for eof - some platforms (x86 without full ACPI) halt but don't exit QEMU
set timeout 5
expect eof
exit 0
"""


def run_cmd(cmd, cwd=None, timeout=None, check=True):
    """Run command and return result."""
    print(f"  $ {' '.join(cmd)}")
    result = subprocess.run(
        cmd,
        cwd=cwd,
        capture_output=True,
        text=True,
        timeout=timeout
    )
    if check and result.returncode != 0:
        print(f"FAILED: {result.stderr[:1000]}")
        return None
    return result


def download_kernel(work_dir):
    """Get kernel source - copy from /deps if available, else download."""
    kernel_dir = work_dir / f"linux-{KERNEL_VERSION}"

    if kernel_dir.exists():
        print(f"Kernel source already exists at {kernel_dir}")
        return kernel_dir

    # Check if pre-downloaded kernel source exists in deps
    deps_kernel = Path(DEPS_DIR) / f"linux-{KERNEL_VERSION}"
    if deps_kernel.exists():
        print(f"Copying kernel source from {deps_kernel}...")
        result = run_cmd(["cp", "-r", str(deps_kernel), str(kernel_dir)], timeout=120)
        if result is not None:
            return kernel_dir
        print("Copy failed, falling back to download...")

    print(f"Downloading Linux {KERNEL_VERSION}...")
    tarball = work_dir / f"linux-{KERNEL_VERSION}.tar.xz"

    result = run_cmd(["wget", "-q", KERNEL_URL, "-O", str(tarball)], timeout=300)
    if result is None:
        return None

    print("Extracting...")
    result = run_cmd(["tar", "xf", str(tarball)], cwd=work_dir, timeout=120)
    if result is None:
        return None

    tarball.unlink()
    return kernel_dir


def patch_kernel(kernel_dir):
    """Add custom printk message for verification."""
    main_c = kernel_dir / "init" / "main.c"
    content = main_c.read_text()

    marker = 'printk(KERN_INFO "Hello, this is a custom kernel");'
    if marker in content:
        print("Kernel already patched")
        return True

    print("Patching kernel with custom printk...")
    # Insert after boot_cpu_hotplug_init();
    patched = content.replace(
        "boot_cpu_hotplug_init();",
        'boot_cpu_hotplug_init();\n\tprintk(KERN_INFO "Hello, this is a custom kernel");'
    )

    if patched == content:
        print("ERROR: Could not find patch location")
        return False

    main_c.write_text(patched)
    return True


def configure_kernel(kernel_dir, cc, hostcc, arch_config):
    """Configure kernel for minimal boot."""
    make_arch, cross_compile, _, _, _, console, _ = arch_config
    print("Configuring kernel...")

    # Build make arguments
    make_args = ["make", f"CC={cc}", f"HOSTCC={hostcc}"]
    if make_arch:
        make_args.append(f"ARCH={make_arch}")
    if cross_compile:
        make_args.append(f"CROSS_COMPILE={cross_compile}")

    # Start with tinyconfig (minimal config, avoids needing GPU/DRM source)
    result = subprocess.run(
        make_args + ["tinyconfig"],
        cwd=kernel_dir,
        capture_output=True,
        text=True,
        timeout=60
    )
    if result.returncode != 0:
        print(f"defconfig failed: {result.stderr[:500]}")
        return False

    # Enable required options
    scripts_config = kernel_dir / "scripts" / "config"
    required_opts = [
        "TTY", "PRINTK", "BLK_DEV_INITRD",
        # Userspace support
        "BINFMT_ELF", "BINFMT_SCRIPT",
        # Filesystems needed by init
        "PROC_FS", "PROC_SYSCTL", "SYSFS", "DEVTMPFS",
        # Networking (init does ip link set lo up)
        "NET", "UNIX", "INET",
        # SMP support
        "SMP",
    ]

    # Architecture-specific options
    if make_arch == 'arm64':
        required_opts.extend(["SERIAL_AMBA_PL011", "SERIAL_AMBA_PL011_CONSOLE",
                              "COMPAT"])  # busybox-armv8l is ELF32
    elif make_arch == 'riscv':
        required_opts.extend(["SERIAL_8250", "SERIAL_8250_CONSOLE",
                              "SOC_VIRT",  # QEMU virt platform
                              "SERIAL_OF_PLATFORM",  # serial from device tree
                              "RISCV_ISA_FALLBACK",  # parse old "riscv,isa" DT property
                              "SERIAL_EARLYCON_RISCV_SBI",
                              "FPU"])  # busybox-riscv64 uses double-float ABI
    else:
        required_opts.extend(["SERIAL_8250", "SERIAL_8250_CONSOLE",
                              "64BIT",  # busybox-x86_64 is ELF64, tinyconfig defaults to 32-bit
                              "ACPI"])  # needed for QEMU poweroff to actually exit

    for opt in required_opts:
        subprocess.run(
            [str(scripts_config), "--enable", opt],
            cwd=kernel_dir,
            capture_output=True
        )

    # Resolve dependencies
    result = subprocess.run(
        make_args + ["olddefconfig"],
        cwd=kernel_dir,
        capture_output=True,
        text=True,
        timeout=60
    )
    if result.returncode != 0:
        print(f"olddefconfig failed: {result.stderr[:500]}")
        return False

    return True


def build_kernel(kernel_dir, cc, hostcc, jobs, arch_config):
    """Build the kernel."""
    make_arch, cross_compile, kernel_image_path, _, _, _, _ = arch_config
    print(f"Building kernel with CC={cc}...")

    make_args = ["make", f"CC={cc}", f"HOSTCC={hostcc}", f"-j{jobs}"]
    if make_arch:
        make_args.append(f"ARCH={make_arch}")
    if cross_compile:
        make_args.append(f"CROSS_COMPILE={cross_compile}")

    result = subprocess.run(
        make_args,
        cwd=kernel_dir,
        capture_output=True,
        text=True,
        timeout=1800  # 30 min timeout
    )

    if result.returncode != 0:
        print(f"Build failed!")
        print(f"STDOUT (last 2000 chars): {result.stdout[-2000:]}")
        print(f"STDERR (last 2000 chars): {result.stderr[-2000:]}")
        return False

    kernel_image = kernel_dir / kernel_image_path
    if not kernel_image.exists():
        print(f"ERROR: Kernel image not found at {kernel_image}")
        return False

    print(f"Kernel built: {kernel_image}")
    return True


def build_initramfs(kernel_dir, ramfs_dir, work_dir, arch_config):
    """Build initramfs using kernel's gen_init_cpio."""
    _, _, _, _, _, _, busybox_name = arch_config
    print(f"Building initramfs with {busybox_name}...")

    gen_init_cpio = kernel_dir / "usr" / "gen_init_cpio"
    if not gen_init_cpio.exists():
        print(f"ERROR: {gen_init_cpio} not found")
        return None

    ramfs_path = Path(ramfs_dir)
    busybox_path = ramfs_path / busybox_name
    if not busybox_path.exists():
        print(f"ERROR: {busybox_path} not found")
        return None

    init_script = ramfs_path / "basic-init"
    if not init_script.exists():
        print(f"ERROR: {init_script} not found")
        return None

    # Create architecture-specific initramfs list
    initramfs_list_content = f"""dir /dev 0755 0 0
nod /dev/console 0600 0 0 c 5 1
dir /root 0700 0 0
dir /bin 0755 0 0
dir /sbin 0755 0 0
file /init basic-init 0755 0 0
file /bin/busybox {busybox_name} 0755 0 0
"""

    # Write temp initramfs.list
    initramfs_list = Path(work_dir) / "initramfs.list"
    initramfs_list.write_text(initramfs_list_content)

    # Write to work_dir (writable) instead of ramfs_dir (may be read-only)
    output_cpio = Path(work_dir) / "initramfs.cpio"
    output_gz = Path(work_dir) / "initramfs.cpio.gz"

    # Generate cpio
    result = subprocess.run(
        [str(gen_init_cpio), str(initramfs_list)],
        cwd=ramfs_path,
        capture_output=True,
        timeout=30
    )
    if result.returncode != 0:
        print(f"gen_init_cpio failed: {result.stderr[:500]}")
        return None

    output_cpio.write_bytes(result.stdout)

    # Compress
    subprocess.run(["gzip", "-9f", str(output_cpio)], check=True)

    print(f"Initramfs built: {output_gz}")
    return output_gz


def boot_and_verify(kernel_path, initrd_path, arch_config):
    """Boot kernel in QEMU and verify custom message."""
    _, _, _, qemu_cmd, qemu_args, console, _ = arch_config
    print(f"Booting kernel in QEMU ({qemu_cmd})...")

    script = get_expect_script(qemu_cmd, qemu_args, kernel_path, initrd_path, console)

    with tempfile.NamedTemporaryFile(mode='w', suffix='.exp', delete=False) as f:
        f.write(script)
        expect_file = f.name

    try:
        result = subprocess.run(
            ["expect", "-f", expect_file],
            capture_output=True,
            text=True,
            timeout=240  # longer timeout for emulated architectures
        )

        print(f"QEMU output:\n{result.stdout}")

        if "SUCCESS: Found verified custom kernel message" in result.stdout:
            return True
        elif "is a verified custom kernel" in result.stdout:
            return True
        else:
            print(f"STDERR: {result.stderr}")
            return False

    except subprocess.TimeoutExpired:
        print("ERROR: QEMU boot timed out")
        return False
    finally:
        os.unlink(expect_file)


def main():
    parser = argparse.ArgumentParser(description='Verify compiler can build bootable Linux kernel')
    parser.add_argument('--compiler', required=True, help='C compiler to use (e.g., gcc, aarch64-linux-gnu-gcc)')
    parser.add_argument('--arch', choices=['x86', 'arm', 'riscv'], default=None,
                        help='Target architecture: x86, arm (arm64), riscv (riscv64). Auto-detected from compiler name if not specified.')
    parser.add_argument('--hostcc', default='gcc', help='Host C compiler (default: gcc)')
    parser.add_argument('--work-dir', default='/tmp/kernel-test', help='Working directory')
    parser.add_argument('--ramfs-dir', default=RAMFS_DIR, help='Ramfs directory with busybox')
    parser.add_argument('--jobs', type=int, default=min(os.cpu_count(), MAX_JOBS), help='Parallel build jobs (max 32)')
    parser.add_argument('--skip-build', action='store_true', help='Skip build, just boot existing kernel')
    parser.add_argument('--timeout', type=int, default=436,
                        help='Timeout in seconds (default: 436)')

    args = parser.parse_args()

    def timeout_handler(signum, frame):
        elapsed = time.time() - start_time
        print(f"\nTIMEOUT: Exceeded {args.timeout}s limit (ran for {elapsed:.1f}s)")
        sys.exit(1)
    start_time = time.time()
    signal.signal(signal.SIGALRM, timeout_handler)
    signal.alarm(args.timeout)

    arch = args.arch
    if arch is None:
        arch = detect_arch_from_compiler(args.compiler)
    if arch is None:
        arch = 'x86'

    arch_config = ARCH_CONFIG[arch]
    make_arch, cross_compile, kernel_image_path, qemu_cmd, _, _, _ = arch_config

    work_dir = Path(args.work_dir)
    work_dir.mkdir(parents=True, exist_ok=True)

    kernel_dir = work_dir / f"linux-{KERNEL_VERSION}"
    kernel_image = kernel_dir / kernel_image_path
    initrd = work_dir / "initramfs.cpio.gz"

    print(f"=== Kernel Build Verification ===")
    print(f"Compiler: {args.compiler}")
    print(f"Arch:     {arch}" + (f" (ARCH={make_arch})" if make_arch else ""))
    print(f"Host CC:  {args.hostcc}")
    print(f"QEMU:     {qemu_cmd}")
    print(f"Work dir: {work_dir}")
    print(f"Jobs:     {args.jobs}")
    print(f"=================================\n")

    if not args.skip_build:
        # Download
        kernel_dir = download_kernel(work_dir)
        if kernel_dir is None:
            print("FAILED: Could not download kernel")
            sys.exit(1)

        # Patch
        if not patch_kernel(kernel_dir):
            print("FAILED: Could not patch kernel")
            sys.exit(1)

        # Configure
        if not configure_kernel(kernel_dir, args.compiler, args.hostcc, arch_config):
            print("FAILED: Could not configure kernel")
            sys.exit(1)

        # Build
        if not build_kernel(kernel_dir, args.compiler, args.hostcc, args.jobs, arch_config):
            print("FAILED: Could not build kernel")
            sys.exit(1)

        # Build initramfs
        initrd = build_initramfs(kernel_dir, args.ramfs_dir, work_dir, arch_config)
        if initrd is None:
            print("FAILED: Could not build initramfs")
            sys.exit(1)

    # Verify paths exist
    if not kernel_image.exists():
        print(f"ERROR: Kernel not found at {kernel_image}")
        sys.exit(1)
    if not initrd.exists():
        print(f"ERROR: Initramfs not found at {initrd}")
        sys.exit(1)

    # Boot and verify
    if boot_and_verify(kernel_image, initrd, arch_config):
        print("\n=== SUCCESS ===")
        print("Kernel boots and custom message verified!")
        sys.exit(0)
    else:
        print("\n=== FAILED ===")
        print("Kernel did not boot correctly or message not found")
        sys.exit(1)


if __name__ == '__main__':
    main()
