type Image = {
    readonly width: number
    readonly height: number
    readonly data: Uint8Array
}

const imnew = (width: number, height: number): Image => {
    const data = new Uint8Array(width * height * 4)
    data.fill(255)
    return { width, height, data }
}

const imcopy = (img: Image): Image => {
    return { width: img.width, height: img.height, data: new Uint8Array(img.data) }
}

export {
    Image,
    imnew,
    imcopy
}