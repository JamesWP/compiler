use std::fs;

use itertools::Itertools;

fn go(name: &str, command: &mut std::process::Command) -> bool {
    let args: Vec<_> = command.get_args().map(|os|os.to_string_lossy().to_string()).collect();
    let program = command.get_program().to_string_lossy().to_string();

    let result = if let Ok(output) = command.output() {
        if output.status.code().unwrap_or(1) == 0 {
            Ok(())
        } else {
            Err((
                    std::str::from_utf8(&output.stdout).map(|s|s.to_owned()).unwrap_or(format!("bad utf8 output")),
                    std::str::from_utf8(&output.stderr).map(|s|s.to_owned()).unwrap_or(format!("bad utf8 output"))
                ))
        }
    } else {
        Err(("".to_owned(), format!("Unable to start program")))
    };


    println!("[{:<10}] {}: {} {:?}", name, if result.is_ok() { "SUCCESS"} else {"FAILED"}, program, args);

    if let Err((ref stdout, ref stderr)) = result {
        for line in stdout.lines() {
            println!("[{:<10}]: {}", "stdout", line);
        }
        for line in stderr.lines() {
            println!("[{:<10}]: {}", "stderr", line);
        }
    }

    result.is_ok()
}

fn native_compile(source_file: &str, object_file: &str) -> bool {
    go("NATIVE CC", std::process::Command::new("gcc").arg("-c").arg(source_file).arg("-o").arg(object_file))
}

fn link(executable_file: &str, object_files: &[String]) -> bool {
    go("LINK", std::process::Command::new("gcc").args(object_files).arg("-o").arg(executable_file))
}

fn run(executable_file: &str) -> bool {
    go("RUN", std::process::Command::new(executable_file).env_clear())
}

pub fn test() -> u32 {
    let mut entries: Vec<_> = fs::read_dir("examples")
        .unwrap()
        .map(|res| res.map(|e|e.path()))
        .filter_map(std::result::Result::ok)
        .map(|e|e.file_name().unwrap().to_owned().into_string().unwrap())
        .collect();

    entries.sort(); 

    let ex = | e: &String | {
        let (num, name) = e.split_once('_').unwrap();
        let ex_num: u64 = num.parse().unwrap();
        ex_num
    };

    let examples = entries.into_iter().group_by(|a,| ex(a));

    let output_dir = "target";
    std::fs::create_dir(output_dir);

    let mut has_failed = false;

    for (key, group) in &examples {
        println!("Example {}", key);

        println!("Building Reference");

        let output = format!("{}/example_{}_ref", output_dir, key);

        let mut objects = vec![];
        for item in group {
            if let Some(base) = item.strip_suffix(".c") {
                let object = format!("{}/{}.o", output_dir, base);
                if !native_compile(&format!("examples/{}", item), &object) {
                    has_failed = true;
                    break;
                }
                objects.push(object);
            }
        }

        if !link(&output, &objects) {
            has_failed = true;
            continue;
        }

        if !run(&output) {
            has_failed = true;
            continue;
        }
        
        println!();
    }

    if has_failed {
        return 1;
    } else {
        return 0;
    }
}