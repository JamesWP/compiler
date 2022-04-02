#[cfg(test)]
mod compiler_unit_tests {
    use crate::{compile, CompilerOptions};
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

    fn ref_compile(source_file: &str, object_file: &str) -> bool {
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

    fn test_compile(source_file: &str, object_file: &str) -> bool {
        let options = CompilerOptions {
            filename: source_file.to_owned(),
            output_filename: object_file.to_owned(),
            debug_lex: false,
            debug_ast: false,
            debug_pp: false,
        };

        let result = compile(&options);

        // stdout for testing compile
        {
            let program = "target/debug/compiler";
            let args = [source_file, "-o", object_file];
            println!(
                "[{:<10}] {}: {} {}",
                "COMPILER",
                if result.is_ok() { "SUCCESS" } else { "FAILED" },
                program,
                args.join(" ")
            );
        }

        result.is_ok()
    }

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

    fn run(executable_file: &str, input_arguments: &[&str], output_bytes: &mut String) -> bool {
        go(
            "RUN",
            std::process::Command::new(executable_file)
                .env_clear()
                .args(input_arguments),
            output_bytes,
        )
    }

    macro_rules! compile {
        (compare $program:ident $($filename:expr)+ $(; argv $($arg:tt)+)?) => {
            #[test]
            pub fn $program() {
                println!("=============================");
                println!("======== Example {:02} =========", stringify!($program));

                let ref_output = compile!(do_run "NATIVE" "ref" ref_compile (stringify!($program)) $(file $filename)* $(; $($arg)+)?);
                let test_output = compile!(do_run "TESTING" "test" test_compile (stringify!($program)) $(file $filename)* $(; $($arg)+)?);

                assert_eq!(ref_output, test_output, "Output should be the same");
            }
        };
        (do_run $title:tt $suffix:tt $compile_func:tt $program:tt $(file $filename:expr)* $(; $($arg:tt)+)?) => {
            {
                println!("----------- Building {} --------------", $title);
                let program_name = format!("target/{}_{}", $program, $suffix);
                let mut output = String::new();
                assert!(link(&program_name, &[
                    $(
                        {
                            let input_name = format!("examples/{}.c", $filename);
                            let object_name = format!("target/{}_ref.o", $filename);
                            assert!($compile_func(&input_name, &object_name), "compiler exited with non zero exit");
                            object_name
                        },
                    )*
                ]), "Link failure for {}", program_name);
                assert!(run(&program_name, &[$($($arg,)+)?], &mut output), "program exited with non zero exit code");
                output
            }
        };
    }

    compile!(compare example_01 "01_main" "01_simple");
    compile!(compare example_02 "02_main" "02_addition");
    compile!(compare example_03 "03_main" "03_variables");
    compile!(compare example_04 "04_main" "04_functions");
    compile!(compare example_05 "05_main");
    compile!(compare example_06 "06_arith");
    compile!(compare example_07 "07_selection");
    compile!(compare example_08 "08_while");
    compile!(compare example_09 "09_tests");
    compile!(compare example_10 "10_test");
    compile!(compare example_11 "11_scopes");
    compile!(compare example_12 "12_print_args"; argv "testing" "testing again");
    compile!(compare example_13 "13_loops");
    compile!(compare example_14 "14_preprocess_subst");
    compile!(compare example_15 "15_aoc_2015_1");
    compile!(compare example_16 "16_strlen");
} // mod compiler_unit_tests
