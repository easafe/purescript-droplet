import { execSync } from "child_process";

export function build_(pkg) {
  try {
    execSync(`spago build -p "${pkg}"`, {
      stdio: ["ignore", "pipe", "pipe"],
    });

    return { exitCode: 0, stderr: "" };
  } catch (err) {
    return {
      exitCode: err.status ?? 1,
      stderr: (err.stderr ?? Buffer.from("")).toString(),
    };
  }
};