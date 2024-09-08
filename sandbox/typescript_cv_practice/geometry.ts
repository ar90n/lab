type Point = {
    readonly x: number
    readonly y: number
}

type HomogeneousPoint = {
    readonly x: number
    readonly y: number
    readonly w: number
}

type Line = {
    readonly nx: number
    readonly ny: number
    readonly c: number
}

type LineSegment = {
    readonly src: Point
    readonly dst: Point
}

type Rect = {
    readonly origin: Point
    readonly width: number
    readonly height: number
}

type Circle = {
    readonly center: Point
    readonly radius: number
}

const intersection = (l1: Line, l2: Line): Point => {
    const det = l1.nx * l2.ny - l1.ny * l2.nx
    if (det === 0) {
        throw new Error("Lines are parallel")
    }
    const x = (l1.ny * l2.c - l2.ny * l1.c) / det
    const y = (l2.nx * l1.c - l1.nx * l2.c) / det
    return { x, y }
}

const line_from_two_points = (src: Point, dst: Point): Line => {
    const dy = dst.y - src.y
    const dx = dst.x - src.x
    const norm = Math.hypot(dx, dy)
    const nx = -dy / norm
    const ny = dx / norm
    const c = -nx * src.x - ny * src.y
    return { nx, ny, c }
}

const lines_from_rect = (rect: Rect): Line[] => {
    const p1 = { x: rect.origin.x, y: rect.origin.y }
    const p2 = { x: rect.origin.x + rect.width, y: rect.origin.y }
    const p3 = { x: rect.origin.x + rect.width, y: rect.origin.y + rect.height }
    const p4 = { x: rect.origin.x, y: rect.origin.y + rect.height }
    return [
        line_from_two_points(p1, p2),
        line_from_two_points(p2, p3),
        line_from_two_points(p3, p4),
        line_from_two_points(p4, p1)
    ]
}

const contain = (rect: Rect, point: Point): boolean => {
    const left_top_x = rect.origin.x
    const left_top_y = rect.origin.y
    const right_buttom_x = rect.origin.x + rect.width
    const right_buttom_y = rect.origin.y + rect.height
    return left_top_x <= point.x && point.x < right_buttom_x && left_top_y <= point.y && point.y < right_buttom_y
}

export {
    Point,
    HomogeneousPoint,
    Line,
    LineSegment,
    Rect,
    Circle,
    intersection,
    line_from_two_points,
    lines_from_rect,
    contain
}