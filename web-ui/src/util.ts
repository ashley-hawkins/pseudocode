export function valueToString(val: any): string {
    if (val instanceof Array) {
        return `[${val.map(valueToString).join(', ')}]`
    }
    return String(val)
}
