type Color = {
    readonly r: number
    readonly g: number
    readonly b: number
}

const Color = {
    red: { r: 255, g: 0, b: 0 },
    green: { r: 0, g: 255, b: 0 },
    blue: { r: 0, g: 0, b: 255 },
    black: { r: 0, g: 0, b: 0 },
    white: { r: 255, g: 255, b: 255 },
    yellow: { r: 255, g: 255, b: 0 },
    magenta: { r: 255, g: 0, b: 255 },
    cyan: { r: 0, g: 255, b: 255 },
    gray: { r: 128, g: 128, b: 128 },
    purple: { r: 128, g: 0, b: 128 },
    orange: { r: 255, g: 165, b: 0 },
    pink: { r: 255, g: 192, b: 203 },
    brown: { r: 165, g: 42, b: 42 },
    teal: { r: 0, g: 128, b: 128 },
    navy: { r: 0, g: 0, b: 128 },
    olive: { r: 128, g: 128, b: 0 },
    maroon: { r: 128, g: 0, b: 0 }
}

export {
    Color
}