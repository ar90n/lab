import { display } from "tslab"
import Jimp from "jimp"

import { Image } from "./image"

const imshow = async (img: Image) => {
    const i = new Jimp({ width: img.width, height: img.height, data: Buffer.from(img.data) })
    display.png((await i.getBufferAsync(i.getMIME())))
}
const imread = async (path: string): Promise<Image> => {
    const image = await Jimp.read(path)
    const buf = new Uint8Array(image.bitmap.data)
    return { width: image.bitmap.width, height: image.bitmap.height, data: buf }
}

const imwrite = async (path: string, img: Image) => {
    const i = new Jimp({ width: img.width, height: img.height, data: Buffer.from(img.data) })
    await i.writeAsync(path)
}

export {
    imshow,
    imread,
    imwrite
}