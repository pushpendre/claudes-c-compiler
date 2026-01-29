use ccc::driver::Driver;

fn main() {
    // Large generated C files (e.g. Bison parsers) can cause deep recursion
    // in our recursive descent parser and IR lowering. Spawn with a larger stack.
    const STACK_SIZE: usize = 64 * 1024 * 1024; // 64 MB
    let builder = std::thread::Builder::new().stack_size(STACK_SIZE);
    let handler = builder.spawn(real_main).expect("failed to spawn main thread");
    let result = handler.join();
    match result {
        Ok(Ok(())) => {}
        Ok(Err(e)) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
        Err(_) => {
            // Thread panicked (e.g., stack overflow in recursive descent parser)
            std::process::exit(1);
        }
    }
}

fn real_main() -> Result<(), String> {
    let args: Vec<String> = std::env::args().collect();
    let mut driver = Driver::new();

    if driver.parse_cli_args(&args)? {
        return Ok(());
    }

    if !driver.has_input_files() {
        return Err("no input files".to_string());
    }

    driver.run()
}
