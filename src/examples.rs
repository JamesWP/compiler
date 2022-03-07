#[cfg(test)]
fn go(name: &str, command: &mut std::process::Command, program_output: &mut String) -> bool {
    let args: Vec<_> = command
        .get_args()
        .map(|os| format!("{:?}", os.to_string_lossy().to_string()))
        .collect();
    let program = command.get_program().to_string_lossy().to_string();

    let result = if let Ok(output) = command.output() {
        let stdout = std::str::from_utf8(&output.stdout)
            .map(|s| s.to_owned())
            .unwrap_or(format!("bad utf8 output"));

        let stderr = std::str::from_utf8(&output.stderr)
            .map(|s| s.to_owned())
            .unwrap_or(format!("bad utf8 output"));

        let status_code = output.status.code().unwrap_or(1);
        if status_code == 0 {
            *program_output = format!("{}{}", stdout, stderr);
            Ok(())
        } else {
            println!("RC {}", status_code);
            Err((stdout, stderr))
        }
    } else {
        Err(("".to_owned(), format!("Unable to start program")))
    };

    println!(
        "[{:<10}] {}: {} {}",
        name,
        if result.is_ok() { "SUCCESS" } else { "FAILED" },
        program,
        args.join(" ")
    );

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

#[cfg(test)]
fn native_compile(source_file: &str, object_file: &str) -> bool {
    let mut output = String::new();
    go(
        "NATIVE CC",
        std::process::Command::new("gcc")
            .arg("-c")
            .arg(source_file)
            .arg("-o")
            .arg(object_file),
        &mut output,
    )
}

#[cfg(test)]
fn compile(source_file: &str, object_file: &str) -> bool {
    let mut output = String::new();
    go(
        "COMPILER",
        std::process::Command::new("target/debug/compiler")
            .arg(source_file)
            .arg("-o")
            .arg(object_file),
        &mut output,
    )
}

#[cfg(test)]
fn link(executable_file: &str, object_files: &[String]) -> bool {
    let mut output = String::new();
    go(
        "LINK",
        std::process::Command::new("gcc")
            .args(object_files)
            .arg("-o")
            .arg(executable_file),
        &mut output,
    )
}

#[cfg(test)]
fn run(executable_file: &str, input_arguments: &[String], output_bytes: &mut String) -> bool {
    go(
        "RUN",
        std::process::Command::new(executable_file)
            .env_clear()
            .args(input_arguments),
        output_bytes,
    )
}

#[cfg(test)]
use itertools::Itertools;

#[cfg(test)]
use std::fs;

#[cfg(test)]
use std::io::BufRead;

#[test]
pub fn test() {
    let mut build_output = String::new();
    assert!(go(
        "BUILD",
        std::process::Command::new("cargo").arg("build"),
        &mut build_output
    ));

    let mut entries: Vec<_> = fs::read_dir("examples")
        .unwrap()
        .map(|res| res.map(|e| e.path()))
        .filter_map(std::result::Result::ok)
        .map(|e| e.file_name().unwrap().to_owned().into_string().unwrap())
        .collect();

    entries.sort();

    let ex = |e: &String| {
        let (num, _name) = e.split_once('_').unwrap();
        let ex_num: u64 = num.parse().unwrap();
        ex_num
    };

    let examples = entries.into_iter().group_by(|a| ex(a));

    let output_dir = "target";
    let _unused = std::fs::create_dir(output_dir);

    let mut has_failed = false;

    for (key, group) in &examples {
        if has_failed {
            break; // no hard feelings
        }
        let mut has_this_failed = false;
        println!("=============================");
        println!("======== Example {:02} =========", key);

        println!("----------- Building REFERENCE");

        let output = format!("{}/example_{:02}_ref", output_dir, key);

        let group: Vec<_> = group.collect();
        let mut input_arguments = vec![];

        let mut objects = vec![];
        for item in group.iter() {
            if let Some(base) = item.strip_suffix(".c") {
                let object = format!("{}/{}.o", output_dir, base);
                let source_file = format!("examples/{}", item);

                let content = std::fs::File::open(source_file.clone()).unwrap();

                for line in std::io::BufReader::new(content).lines() {
                    let line = line.unwrap();
                    if !line.starts_with("// arg: ") {
                        continue;
                    }
                    let arg = line[8..].to_owned();
                    println!("Arg: {}", arg);
                    input_arguments.push(arg);
                }

                if !native_compile(&source_file, &object) {
                    has_this_failed = true;
                    break;
                }
                objects.push(object);
            }
        }

        if has_this_failed {
            has_failed = true;
            continue;
        }

        if !link(&output, &objects) {
            has_failed = true;
            continue;
        }

        let mut ref_output_bytes = String::new();
        if !run(&output, &input_arguments, &mut ref_output_bytes) {
            has_failed = true;
            continue;
        }

        println!("----------- Building TEST");

        let output = format!("{}/example_{:02}_test", output_dir, key);

        let mut objects = vec![];
        for item in group.iter() {
            if let Some(base) = item.strip_suffix(".c") {
                let object = format!("{}/{}.o", output_dir, base);
                if base.contains("driver") {
                    if !native_compile(&format!("examples/{}", item), &object) {
                        has_failed = true;
                        break;
                    }
                } else {
                    if !compile(&format!("examples/{}", item), &object) {
                        has_failed = true;
                        break;
                    }
                }
                objects.push(object);
            }
        }

        if has_this_failed {
            has_failed = true;
            continue;
        }

        if !link(&output, &objects) {
            has_failed = true;
            continue;
        }

        let mut test_output_bytes = String::new();
        if !run(&output, &input_arguments, &mut test_output_bytes) {
            has_failed = true;
            continue;
        }

        if ref_output_bytes != test_output_bytes {
            has_failed = true;
            println!("Ref:");
            println!("{}", ref_output_bytes);
            println!("Test:");
            println!("{}", test_output_bytes);
            continue;
        }

        println!();
    }

    assert_eq!(has_failed, false);
}
