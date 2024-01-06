from manim import *

class Prob1(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 1.05, 0.1],
      axis_config={"numbers_to_include": [0, 0.1, 1]},
      x_length=6,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="\\mathbb{P}(\\mathrm{success})")
    self.play(Write(axes), Write(axes_labels))

    p_graph = axes.plot(lambda p: p, color=WHITE, x_range=[0, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{default}")
    self.play(Create(p_graph), Create(p_graph_label))
    self.wait(3)

class Prob2(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 1.05, 0.1],
      axis_config={"numbers_to_include": [0, 0.1, 1]},
      x_length=6,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="\\mathbb{P}(\\mathrm{success})")
    self.add(axes, axes_labels)

    p_graph = axes.plot(lambda p: p, color=WHITE, x_range=[0, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{default}", x_val=-0.3)
    self.add(p_graph, p_graph_label)
    self.wait(1)

    adv_graph = axes.plot(lambda p: 2*p - p*p, x_range=[0, 1], color=GREEN)
    adv_graph_label = axes.get_graph_label(adv_graph, label="\\mathrm{advantage}", direction=RIGHT)
    self.play(Create(adv_graph), Create(adv_graph_label))
    self.wait(3)

class Prob3(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 1.05, 0.1],
      axis_config={"numbers_to_include": [0, 0.1, 1]},
      x_length=6,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="\\mathbb{P}(\\mathrm{success})")
    self.add(axes, axes_labels)

    p_graph = axes.plot(lambda p: p, color=WHITE, x_range=[0, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{default}", x_val=-0.3)
    self.add(p_graph, p_graph_label)

    adv_graph = axes.plot(lambda p: 2*p - p*p, x_range=[0, 1], color=GREEN)
    adv_graph_label = axes.get_graph_label(adv_graph, label="\\mathrm{advantage}", direction=RIGHT)
    self.add(adv_graph, adv_graph_label)
    self.wait(1)

    dis_graph = axes.plot(lambda p: p*p, x_range=[0, 1], color=RED)
    dis_graph_label = axes.get_graph_label(dis_graph, label="\\mathrm{disadvantage}", direction=LEFT)
    self.play(Create(dis_graph), Create(dis_graph_label))
    self.wait(3)

class ProbAll(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.05],
      y_range = [0, 1.05, 0.05],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 0.25, 0.5, 0.75, 1]},
      x_length=6,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="\\mathbb{P}(\\mathrm{success})")
    self.play(Create(axes), Create(axes_labels))
    self.wait(1)

    p_graph = axes.plot(lambda p: p, color=WHITE, x_range=[0, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{default}", x_val=-0.3)

    adv_graph = axes.plot(lambda p: 2*p - p*p, x_range=[0, 1], color=GREEN)
    adv_graph_label = axes.get_graph_label(adv_graph, label="\\mathrm{advantage}", direction=RIGHT)

    dis_graph = axes.plot(lambda p: p*p, x_range=[0, 1], color=RED)
    dis_graph_label = axes.get_graph_label(dis_graph, label="\\mathrm{disadvantage}", direction=LEFT)

    self.play(
      Create(p_graph), Create(p_graph_label),
      Create(adv_graph), Create(adv_graph_label),
      Create(dis_graph), Create(dis_graph_label),
    )
    self.wait(2)

    adv_line = axes.get_lines_to_point(axes.c2p(0.5, 0.75))
    dis_line = axes.get_lines_to_point(axes.c2p(0.5, 0.25))
    self.play(Create(adv_line), Create(dis_line))
    self.wait(5)

class ProbUpside(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 2.05, 0.5],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 1, 1.5, 2]},
      x_length=5,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="\\mathbb{P}(\\mathrm{success})/p")
    self.add(axes, axes_labels)
    self.wait(1)

    p_graph = axes.plot(lambda p: 2 if p < 0.0001 else (2*p - p*p) / p, color=GREEN, x_range=[0, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{advantage}", direction=RIGHT)
    self.play(Create(p_graph), Create(p_graph_label))
    self.wait(1)

    line1 = axes.get_lines_to_point(axes.c2p(1, 1))
    d11 = Dot(axes.c2p(1, 1), color=PINK)
    line05 = axes.get_lines_to_point(axes.c2p(0.5, 1.5))
    d05 = Dot(axes.c2p(0.5, 1.5), color=PINK)
    self.play(Create(line1), Create(line05))
    self.add(d11, d05)
    self.wait(5)

class ProbDownside(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 20.05, 2],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 1, 2, 20]},
      x_length=5,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="p/\\mathbb{P}(\\mathrm{success})")
    self.add(axes, axes_labels)
    self.wait(1)

    p_graph = axes.plot(lambda p: 1/p, color=RED, x_range=[0.05, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{disadvantage}", direction=UP)
    self.play(Create(p_graph), Create(p_graph_label))
    self.wait(1)

    line1 = axes.get_lines_to_point(axes.c2p(1, 1))
    d11 = Dot(axes.c2p(1, 1), color=PINK)
    line05 = axes.get_lines_to_point(axes.c2p(0.5, 2))
    d05 = Dot(axes.c2p(0.5, 2), color=PINK)
    self.play(Create(line1), Create(line05))
    self.add(d11, d05)
    self.wait(5)

class ProbDownsideLog(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 1, 2, 20], "scaling": LogBase(custom_labels=False)},
      x_length=5,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="log(p/\\mathbb{P}(\\mathrm{success}))")
    self.add(axes, axes_labels)
    self.wait(1)

    p_graph = axes.plot(lambda p: 1/p, color=RED, x_range=[0.05, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="\\mathrm{disadvantage}", direction=UP)
    self.play(Create(p_graph), Create(p_graph_label))
    self.wait(1)

    line1 = axes.get_lines_to_point(axes.c2p(1, 1))
    d11 = Dot(axes.c2p(1, 1), color=PINK)
    line05 = axes.get_lines_to_point(axes.c2p(0.5, 2))
    d05 = Dot(axes.c2p(0.5, 2), color=PINK)
    self.play(Create(line1), Create(line05))
    self.add(d11, d05)
    self.wait(5)

class ProbUpDownSide(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 20.05, 2],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 1, 2, 20]},
      x_length=5,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="")
    self.add(axes, axes_labels)
    self.wait(1)

    adv_graph = axes.plot(lambda p: 2 if p < 0.0001 else (2*p - p*p) / p, color=GREEN, x_range=[0, 1])
    adv_graph_label = axes.get_graph_label(adv_graph, label="\\mathrm{advantage}", direction=UP)

    dis_graph = axes.plot(lambda p: 1/p, color=RED, x_range=[0.05, 1])
    dis_graph_label = axes.get_graph_label(dis_graph, label="\\mathrm{disadvantage}", x_val=0.01)

    line1 = axes.get_lines_to_point(axes.c2p(1, 1))
    d11 = Dot(axes.c2p(1, 1), color=PINK)
    line05 = axes.get_lines_to_point(axes.c2p(0.5, 2))
    d05 = Dot(axes.c2p(0.5, 2), color=PINK)
    self.play(Create(adv_graph), Create(adv_graph_label), Create(dis_graph), Create(dis_graph_label), Create(line1), Create(line05))
    self.add(d11, d05)

    self.wait(5)

class ProbUpDownSideLog(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      # y_range = [0, 20.05, 2],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 1, 2, 20], "scaling": LogBase(custom_labels=False)},
      x_length=5,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="")
    self.add(axes, axes_labels)
    self.wait(1)

    adv_graph = axes.plot(lambda p: 2 if p < 0.0001 else (2*p - p*p) / p, color=GREEN, x_range=[0, 1])
    adv_graph_label = axes.get_graph_label(adv_graph, label="\\mathrm{advantage}", direction=UP)

    dis_graph = axes.plot(lambda p: 1/p, color=RED, x_range=[0.05, 1])
    dis_graph_label = axes.get_graph_label(dis_graph, label="\\mathrm{disadvantage}", x_val=0.01)
    line1 = axes.get_lines_to_point(axes.c2p(1, 1))
    d11 = Dot(axes.c2p(1, 1), color=PINK)
    line05 = axes.get_lines_to_point(axes.c2p(0.5, 2))
    d05 = Dot(axes.c2p(0.5, 2), color=PINK)
    self.play(Create(adv_graph), Create(adv_graph_label), Create(dis_graph), Create(dis_graph_label), Create(line1), Create(line05))
    self.add(d11, d05)

    self.wait(5)

class ProbRelative(Scene):
  def construct(self):
    axes = Axes(
      x_range = [0, 1.05, 0.1],
      y_range = [0, 11.5, 1],
      x_axis_config={"numbers_to_include": [0, 0.1, 0.5, 1]},
      y_axis_config={"numbers_to_include": [0, 1, 10]},
      x_length=5,
      y_length=6,
      tips=False
    )
    axes_labels = axes.get_axis_labels(x_label="p", y_label="")
    self.add(axes, axes_labels)
    self.add(Text("Relative impact of disadvantage vs advantage", font_size=28).to_edge(UR))
    self.wait(1)

    p_graph = axes.plot(lambda p: 1 / (p*(2 - p)), color=PINK, x_range=[0.05, 1])
    p_graph_label = axes.get_graph_label(p_graph, label="", direction=RIGHT)
    line1 = axes.get_lines_to_point(axes.c2p(1, 1))
    d11 = Dot(axes.c2p(1, 1), color=PINK)
    self.play(Create(p_graph), Create(p_graph_label), Create(line1))
    self.add(d11)
    self.wait(5)
