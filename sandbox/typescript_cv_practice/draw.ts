import { Point, Rect, Circle, LineSegment, Line, lines_from_rect, intersection } from './geometry'
import { Color } from './color'
import { scanRect, scanLineSegment } from './sampling'
import { Command } from './render'
import { Image } from './image'

const drawRect = (rect: Rect, color: Color): Command => {
    return {
        apply: (buf: Image): Image => {
            scanRect(rect, (pt: Point) => {
                const idx = pt.y * buf.width * 4 + pt.x * 4
                buf.data[idx] = color.r
                buf.data[idx + 1] = color.g
                buf.data[idx + 2] = color.b
            })

            return buf
        }
    }
}

const drawCircle = (circle: Circle, color: Color): Command => {
    return {
        apply: (buf: Image): Image => {
            const bounding_box = {
                origin: {
                    x: circle.center.x - circle.radius,
                    y: circle.center.y - circle.radius
                },
                width: circle.radius * 2,
                height: circle.radius * 2
            }
            scanRect(bounding_box, (pt: Point) => {
                const dx = pt.x - circle.center.x
                const dy = pt.y - circle.center.y
                if (dx * dx + dy * dy < circle.radius * circle.radius) {
                    const idx = pt.y * buf.width * 4 + pt.x * 4
                    buf.data[idx] = color.r
                    buf.data[idx + 1] = color.g
                    buf.data[idx + 2] = color.b
                }
            })

            return buf
        }
    }
}

const drawLineSegment = (line: LineSegment, color: Color): Command => {
    return {
        apply: (buf: Image): Image => {
            scanLineSegment(line, (pt: Point) => {
                const idx = pt.y * buf.width * 4 + pt.x * 4
                buf.data[idx] = color.r
                buf.data[idx + 1] = color.g
                buf.data[idx + 2] = color.b
            })

            return buf
        }
    }
}

const drawLine = (line: Line, color: Color): Command => {
    return {
        apply: (buf: Image): Image => {
            const { width, height } = buf
            const vs = lines_from_rect({ origin: { x: 0, y: 0 }, width, height }).map(l => intersection(l, line)).filter(p => p.x >= 0 && p.x <= buf.width && p.y >= 0 && p.y <= buf.height)
            const src = { x: Math.floor(vs[0].x), y: Math.floor(vs[0].y) }
            const dst = { x: Math.floor(vs[1].x), y: Math.floor(vs[1].y) }
            const line_segment = { src, dst}
            //console.log(line_segment)
            scanLineSegment(line_segment, (pt: Point) => {
                const idx = pt.y * buf.width * 4 + pt.x * 4
                buf.data[idx] = color.r
                buf.data[idx + 1] = color.g
                buf.data[idx + 2] = color.b
            })

            return buf
        }
    }
}

export {
    drawRect,
    drawCircle,
    drawLineSegment,
    drawLine
}