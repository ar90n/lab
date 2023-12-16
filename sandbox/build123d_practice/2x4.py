# %%
from build123d import *
from ocp_vscode import show
# %%
from typing import Literal, TypeAlias
from dataclasses import dataclass
from enum import Enum

# %%
MilliMeter: TypeAlias = float

@dataclass
class Shape:
    width: MilliMeter
    height: MilliMeter

    def __iter__(self):
        yield self.width
        yield self.height

# %%
class HeightSize(Enum):
    ONE = 1
    TWO = 2

    @staticmethod
    def of(value: int) -> 'HeightSize':
        match(value):
            case 1: return HeightSize.ONE
            case 2: return HeightSize.TWO
        raise ValueError(f"Invalid height size: {value}")
    
    def to_mm(self) -> MilliMeter:
        match(self):
            case HeightSize.ONE: return 19.0
            case HeightSize.TWO: return 38.0

class WidthSize(Enum):
    TWO = 2
    THREE = 3
    FOUR = 4
    SIZE = 6
    EIGHT = 8
    TEN = 10

    @staticmethod
    def of(value: int) -> 'WidthSize':
        match(value):
            case 2: return WidthSize.TWO
            case 3: return WidthSize.THREE
            case 4: return WidthSize.FOUR
            case 6: return WidthSize.SIZE
            case 8: return WidthSize.EIGHT
            case 10: return WidthSize.TEN
        raise ValueError(f"Invalid width size: {value}")
    
    def to_mm(self) -> MilliMeter:
        match(self):
            case WidthSize.TWO: return 38.0
            case WidthSize.THREE: return 63.0
            case WidthSize.FOUR: return 89.0
            case WidthSize.SIZE: return 140.0
            case WidthSize.EIGHT: return 184.0
            case WidthSize.TEN: return 235.0
# %%
@dataclass
class Size:
    width: WidthSize
    height: HeightSize

    @staticmethod
    def of(width: int, height: int) -> 'Size':
        return Size(WidthSize.of(width), HeightSize.of(height))
    
    def to_mm(self) -> Shape:
        return Shape(self.width.to_mm(), self.height.to_mm())

# %%
def creatLumber(size: Size, length: float):
    return Box(length, *size.to_mm())

# %%
a = creatLumber(Size.of(4, 2), 200)
RigidJoint(label="outlet", to_part=a, joint_location=Location(a.faces().sort_by(Axis.Z).first.center_location))

b = creatLumber(Size.of(4, 2), 200)
RigidJoint(label="outlet", to_part=b, joint_location=Location(b.faces().sort_by(Axis.Z).last.center_location))

c = creatLumber(Size.of(8, 1), 400)
RigidJoint(label="outlet0", to_part=c, joint_location=Location(-c.faces().sort_by(Axis.X).first.center_location))
RigidJoint(label="outlet1", to_part=c, joint_location=Location(-c.faces().sort_by(Axis.X).last.center_location))

# connect_toを呼び出した側が固定される
a.joints["outlet"].connect_to(c.joints["outlet0"])
c.joints["outlet1"].connect_to(b.joints["outlet"])

# %%
show(a + b + c, render_joints=True)
# %%
def create_h_shaped_leg(height: float, interval: float):
    leg_height = height - 38 
    legs_interval = interval - 2 * 89

    leg_r = creatLumber(Size.of(4, 2), leg_height)
    RigidJoint(
        label="to_apron_u",
        to_part=leg_r,
        joint_location=Location(
            leg_r.faces().sort_by(Axis.Y).first.edges().sort_by(Axis.X).first.center() + (0.0, -0.1, 0.0),
           (0, 0, -90)
        )
    )

    RigidJoint(
        label="to_apron_b",
        to_part=leg_r,
        joint_location=Location(
            leg_r.faces().sort_by(Axis.Y).first.center() + (0.0, -0.1, 0.0),
           (0, 0, -90)
        )
    )

    leg_l = creatLumber(Size.of(4, 2), leg_height)
    RigidJoint(
        label="to_apron_u",
        to_part=leg_l,
        joint_location=Location(
            leg_r.faces().sort_by(Axis.Y).last.edges().sort_by(Axis.X).first.center() + (0.0, 0.1, 0.0),
           (0, 0, -90)
        )
    )

    RigidJoint(
        label="to_apron_b",
        to_part=leg_l,
        joint_location=Location(
            leg_r.faces().sort_by(Axis.Y).last.center() + (0.0, 0.1, 0.0),
           (0, 0, -90)
        )
    )

    apron_u = creatLumber(Size.of(4, 2), legs_interval)
    RigidJoint(
        label="to_leg_r",
        to_part=apron_u,
        joint_location=Location(
            apron_u.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Y).first.center()
        )
    )

    RigidJoint(
        label="to_leg_l",
        to_part=apron_u,
        joint_location=Location(
            apron_u.faces().sort_by(Axis.X).last.edges().sort_by(Axis.Y).first.center(),
        )
    )

    apron_b = creatLumber(Size.of(4, 2), legs_interval)
    RigidJoint(
        label="to_leg_r",
        to_part=apron_b,
        joint_location=Location(
            apron_u.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Y).first.center(),
        )
    )
    RigidJoint(
        label="to_leg_l",
        to_part=apron_b,
        joint_location=Location(
            apron_u.faces().sort_by(Axis.X).last.edges().sort_by(Axis.Y).first.center(),
        )
    )

    leg_r.joints["to_apron_u"].connect_to(apron_u.joints["to_leg_r"])
    leg_r.joints["to_apron_b"].connect_to(apron_b.joints["to_leg_r"])
    apron_u.joints["to_leg_l"].connect_to(leg_l.joints["to_apron_u"])
    apron_b.joints["to_leg_l"].connect_to(leg_l.joints["to_apron_b"])

    ret = leg_r + leg_l + apron_u + apron_b
    return ret

# %%
hleg_l = create_h_shaped_leg(667.0, 520.0)
RigidJoint(
    label="to_appron_lu",
    to_part=hleg_l,
    joint_location=Location(
        hleg_l.faces().sort_by(Axis.Y).first.edges().sort_by(Axis.X).first.vertices().sort_by(Axis.Z).last.center()
    )
)
RigidJoint(
    label="to_appron_ru",
    to_part=hleg_l,
    joint_location=Location(
        hleg_l.faces().sort_by(Axis.Y).last.edges().sort_by(Axis.X).first.vertices().sort_by(Axis.Z).last.center()
    )
)
#RigidJoint(
#    label="to_appron_rb",
#    to_part=hleg_l,
#    joint_location=Location(
#        hleg_l.faces().sort_by(Axis.Z)[-4:].sort_by(Axis.Y).last.edges().sort_by(Axis.X).last.vertices().sort_by(Axis.Z).first.center() - (150.0 + 89.0 / 2.0, 0.0, 0.0)
#    )
#)

hleg_r = create_h_shaped_leg(667.0, 520.0)
RigidJoint(
    label="to_appron_lu",
    to_part=hleg_r,
    joint_location=Location(
        hleg_r.faces().sort_by(Axis.Y).first.edges().sort_by(Axis.X).first.vertices().sort_by(Axis.Z).first.center()
    )
)
RigidJoint(
    label="to_appron_ru",
    to_part=hleg_r,
    joint_location=Location(
        hleg_r.faces().sort_by(Axis.Y).last.edges().sort_by(Axis.X).first.vertices().sort_by(Axis.Z).first.center()
    )
)
RigidJoint(
    label="to_appron_lb",
    to_part=hleg_r,
    joint_location=Location(
        hleg_r.faces().sort_by(Axis.Z)[:4].sort_by(Axis.Y).first.edges().sort_by(Axis.X).last.vertices().sort_by(Axis.Z).last.center() - (150.0 + 89.0 / 2.0, 0.0, 0.0)
    )
)
RigidJoint(
    label="to_appron_rb",
    to_part=hleg_r,
    joint_location=Location(
        hleg_r.faces().sort_by(Axis.Z)[:4].sort_by(Axis.Y).last.edges().sort_by(Axis.X).last.vertices().sort_by(Axis.Z).first.center() - (150.0 + 89.0 / 2.0, 0.0, 0.0)
    )
)


appron_length = 630 - 2 * 38
apron_lu = creatLumber(Size.of(4, 2), appron_length)
RigidJoint(
    label="to_leg_l",
    to_part=apron_lu,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).last.edges().sort_by(Axis.Y).first.vertices().sort_by(Axis.Z).last.center() + (0.1, 0, 0),
        (0, -90, 90)
    )
)
RigidJoint(
    label="to_leg_r",
    to_part=apron_lu,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Y).first.vertices().sort_by(Axis.Z).last.center() + (-0.1, 0, 0),
        (0, -90, 90)
    )
)

apron_lb = creatLumber(Size.of(4, 2), appron_length)
RigidJoint(
    label="to_leg_r",
    to_part=apron_lb,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Z).last.center() - (0.1, 0, 0),
        (0, -90, 90)
    )
)


apron_ru = creatLumber(Size.of(4, 2), appron_length)
RigidJoint(
    label="to_leg_l",
    to_part=apron_ru,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).last.edges().sort_by(Axis.Y).first.vertices().sort_by(Axis.Z).first.center() + (0.1, 0, 0),
        (0, -90, 90)
    )
)
RigidJoint(
    label="to_leg_r",
    to_part=apron_ru,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Y).first.vertices().sort_by(Axis.Z).first.center() + (-0.1, 0, 0),
        (0, -90, 90)
    )
)


apron_rb = creatLumber(Size.of(4, 2), appron_length)
RigidJoint(
    label="to_leg_l",
    to_part=apron_rb,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).last.edges().sort_by(Axis.Z).first.center() + (0.1, 0, 0),
        (0, -90, 90)
    )
)
RigidJoint(
    label="to_leg_r",
    to_part=apron_rb,
    joint_location=Location(
        apron_lu.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Z).first.center() - (0.1, 0, 0),
        (0, -90, 90)
    )
)

hleg_r.joints["to_appron_lu"].connect_to(apron_lu.joints["to_leg_r"])
hleg_r.joints["to_appron_ru"].connect_to(apron_ru.joints["to_leg_r"])
hleg_r.joints["to_appron_rb"].connect_to(apron_rb.joints["to_leg_r"])
hleg_r.joints["to_appron_lb"].connect_to(apron_lb.joints["to_leg_r"])
apron_lu.joints["to_leg_l"].connect_to(hleg_l.joints["to_appron_lu"])
apron_ru.joints["to_leg_l"].connect_to(hleg_l.joints["to_appron_ru"])

#show(hleg_l + hleg_r + apron_lu + apron_ru + apron_rb)
show(hleg_l + hleg_r + apron_lu + apron_lb + apron_ru + apron_rb)
#show(hleg_l + hleg_r, render_joints=True)


# %%
part = hleg_l + hleg_r + apron_lu + apron_lb + apron_ru + apron_rb
view_port_origin=(-100, -50, 30)
visible, hidden = part.project_to_viewport(view_port_origin)
max_dimension = max(*Compound(children=visible + hidden).bounding_box().size)
exporter = ExportSVG(scale=100 / max_dimension)
exporter.add_layer("Visible")
exporter.add_layer("Hidden", line_color=(99, 99, 99), line_type=LineType.ISO_DOT)
exporter.add_shape(visible, layer="Visible")
exporter.add_shape(hidden, layer="Hidden")
exporter.write("part_projection.svg")
# %%
exporter = ExportSVG(unit=Unit.MM, line_weight=0.5)
exporter.add_layer("Layer 1", fill_color=(255, 0, 0), line_color=(0, 0, 255))
exporter.add_shape(part, layer="Layer 1")
exporter.write("output.svg")
# %%
exporter = ExportDXF(unit=Unit.MM, line_weight=0.5)
exporter.add_layer("Layer 1", color=ColorIndex.RED, line_type=LineType.DASHED)
exporter.add_shape(part, layer="Layer 1")
exporter.write("output.dxf")