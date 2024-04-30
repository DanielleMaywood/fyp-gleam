import { exists } from "https://deno.land/std@0.223.0/fs/exists.ts";

function makeWasmPath(projectPath, projectName) {
    return `${projectPath}/build/dev/webassembly/${projectName}/program.wasm`;
}

async function runProject(project) {
    const projectName = `test_${project}`;
    const projectPath = `./${projectName}`;

    if (await exists(`${projectPath}/build`)) {
        await Deno.remove(`${projectPath}/build`, { recursive: true });
    }

    const command = new Deno.Command("cargo", {
        args: ["run", "--", "build"],
        cwd: projectPath,
    });

    const { code } = await command.output();

    if (code !== 0) {
        return { error: "failed to compile" };
    }

    const wasmPath = makeWasmPath(projectPath, projectName);
    const wasmCode = await Deno.readFile(wasmPath);

    let wasmModule;
    try {
        wasmModule = new WebAssembly.Module(wasmCode);
    } catch (reason) {
        return { error: `failed to create module: ${reason}` };
    }

    const wasmInstance = new WebAssembly.Instance(wasmModule);
    const main = wasmInstance.exports[`${projectName}/main`];

    const expected = await Deno.readTextFile(`${projectPath}/expected.txt`);
    const [expectedType, expectedValue] = expected.split(":");

    const toFloat = wasmInstance.exports["gleam/Float$to_float"];
    const toBool = wasmInstance.exports["gleam/Bool$to_bool"];
    const toInt = wasmInstance.exports["gleam/Int$to_int"];

    try {
        let output = main();

        switch (expectedType) {
            case "int":
                output = toInt(output).toString();
                break;
            case "float":
                output = toFloat(output).toString();
                break;
            case "bool":
                output = toBool(output).toString();
                break;
        }

        return {
            output,
            expected: expectedValue.trim(),
        };
    } catch (reason) {
        return {
            error: `crashed: ${reason}`,
        };
    }
}

async function runTest(project) {
    const { error, output, expected } = await runProject(project);

    if (error) {
        console.log(`%c❌ Test '${project}' ${error}`, "color: red;");
        return false;
    }

    if (output !== expected) {
        console.log(`%c❌ Test '${project}' failed`, "color: red;");
        return false;
    }

    console.log(`%c✓  Test '${project}' passed`, "color: green;");
    return true;
}

switch (Deno.args[0]) {
    case "run": {
        const { error, output, expected } = await runProject(Deno.args[1]);
        if (error) {
            console.log(`%c❌ '${error}'`, "color: red;");
        } else if (output === expected) {
            console.log(`%c✓  Output = '${output}'`, "color: green;");
        } else {
            console.log(`%c❌ Output = '${output}'`, "color: red;");
        }
        break;
    }
    case "test": {
        runTest(Deno.args[1]);
        break;
    }
    case "test-all": {
        let testsPassed = 0;
        let testsRan = 0;
        let tests = [];

        for await (const dirEntry of Deno.readDir("./")) {
            if (dirEntry.name.startsWith("test_") && dirEntry.isDirectory) {
                testsRan += 1;

                tests.push(
                    runTest(dirEntry.name.substring("test_".length)).then(
                        (passed) => {
                            if (passed) {
                                testsPassed += 1;
                            }
                        },
                    ),
                );
            }
        }

        await Promise.all(tests);

        console.log(
            `${testsPassed}/${testsRan} tests passed (${((testsPassed / testsRan) * 100).toFixed(2)}%)`,
        );

        break;
    }
}
