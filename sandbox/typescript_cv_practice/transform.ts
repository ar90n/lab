import { Point } from './geometry'

type Transform = {
    readonly a: number
    readonly b: number
    readonly c: number
    readonly d: number
    readonly e: number
    readonly f: number
}

const compose = (t1: Transform, t2: Transform): Transform => {
    return {
        a: t1.a * t2.a + t1.b * t2.d,
        b: t1.a * t2.b + t1.b * t2.e,
        c: t1.a * t2.c + t1.b * t2.f + t1.c,
        d: t1.d * t2.a + t1.e * t2.d,
        e: t1.d * t2.b + t1.e * t2.e,
        f: t1.d * t2.c + t1.e * t2.f + t1.f
    }
}

const apply = (t: Transform, p: Point): Point => {
    return {
        x: t.a * p.x + t.b * p.y + t.c,
        y: t.d * p.x + t.e * p.y + t.f
    }
}

const invert = (t: Transform): Transform => {
    const det = t.a * t.e - t.b * t.d
    return {
        a: t.e / det,
        b: -t.b / det,
        c: (t.b * t.f - t.e * t.c) / det,
        d: -t.d / det,
        e: t.a / det,
        f: (t.d * t.c - t.a * t.f) / det
    }
}

const translate = (dx: number, dy: number): Transform => {
    return {
        a: 1,
        b: 0,
        c: dx,
        d: 0,
        e: 1,
        f: dy
    }
}

const scale = (sx: number, sy: number): Transform => {
    return {
        a: sx,
        b: 0,
        c: 0,
        d: 0,
        e: sy,
        f: 0
    }
}

const rotate = (angle: number): Transform => {
    const cos = Math.cos(angle)
    const sin = Math.sin(angle)
    return {
        a: cos,
        b: -sin,
        c: 0,
        d: sin,
        e: cos,
        f: 0
    }
}

const Identity: Transform = {
    a: 1,
    b: 0,
    c: 0,
    d: 0,
    e: 1,
    f: 0
}

export {
    Transform,
    compose,
    apply,
    invert,
    translate,
    scale,
    rotate,
    Identity
}