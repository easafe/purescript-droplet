export function environmentVariable_(name) {
    return process.env[name] ?? null;
}
