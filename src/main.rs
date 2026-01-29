use ccc::driver::Driver;

fn main() {
    // Large generated C files (e.g. Bison parsers) can cause deep recursion
    // in our recursive descent parser and IR lowering. Spawn with a larger stack.
    const STACK_SIZE: usize = 64 * 1024 * 1024; // 64 MB
    let builder = std::thread::Builder::new().stack_size(STACK_SIZE);
    let handler = builder.spawn(|| { real_main(); }).expect("failed to spawn main thread");
    let result = handler.join();
    if result.is_err() {
        std::process::exit(1);
    }
}

fn real_main() {
    let args: Vec<String> = std::env::args().collect();
    let mut driver = Driver::new();

    if driver.parse_cli_args(&args) {
        return;
    }

    if driver.input_files.is_empty() {
        eprintln!("error: no input files");
        std::process::exit(1);
    }

    match driver.run() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
    }
}
