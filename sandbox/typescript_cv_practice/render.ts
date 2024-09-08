import { Image, imcopy } from './image'

interface Command {
    apply(Image): Image
}

const render = (img: Image, commands: Command[]): Image => {
    let buf_img = imcopy(img)
    for (const command of commands) {
        buf_img = command.apply(buf_img)
    }
    return buf_img
}

export {
    render,
    Command
}