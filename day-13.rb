# https://adventofcode.com/2018/day/13 in Ruby.

class TurnPlan; end
class Left     < TurnPlan; def next; Straight.new; end; end
class Straight < TurnPlan; def next; Right.new;    end; end
class Right    < TurnPlan; def next; Left.new;     end; end

class Direction
  def turn(signal)
    case signal
      when '/' then slash
      when '\\' then backslash
      when Straight then self
      when Left then left
      when Right then right
    end
  end

  def self.parse(character)
    case character
      when '^' then North.new
      when '>' then East.new
      when 'v' then South.new
      when '<' then West.new
    end
  end
end

class North < Direction
  def x; 0; end
  def y; -1; end
  def left;      West.new; end   # ⮢
  def right;     East.new; end   # ⮣
  def slash;     East.new; end   # ⮣
  def backslash; West.new; end   # ⮢
end

class East < Direction
  def x; 1; end
  def y; 0; end
  def left;      North.new; end  # ⮥
  def right;     South.new; end  # ⮧
  def slash;     North.new; end  # ⮥
  def backslash; South.new; end  # ⮧
end

class South < Direction
  def x; 0; end
  def y; 1; end
  def left;      East.new; end   # ⮡
  def right;     West.new; end   # ⮠
  def slash;     West.new; end   # ⮠
  def backslash; East.new; end   # ⮡
end

class West < Direction
  def x; -1; end
  def y; 0; end
  def left;      South.new; end  # ⮦
  def right;     North.new; end  # ⮤
  def slash;     South.new; end  # ⮦
  def backslash; North.new; end  # ⮤
end

class Cart
  attr_reader :x, :y

  def initialize(x, y, dir)
    @x, @y, @dir, @plan = x, y, dir, Left.new
  end

  def ahead
    @x += @dir.x
    @y += @dir.y
  end

  def turn(signal)
    case signal
      when '+' then turn(@plan); @plan = @plan.next
      else @dir = @dir.turn(signal)
    end
  end
end

carts = []
world = Hash.new(nil)

ARGF.each_with_index do |line, y|
  line.chars.each_with_index do |ch, x|
    world[[x, y]] = ch if '+\/'.include?(ch)
    (dir = Direction.parse(ch)) and carts.push(Cart.new(x, y, dir))
  end
end

collisions = []

until carts.size <= 1
  # Simulate a tick.
  carts.sort_by { |c| [c.y, c.x] }.each do |c|
    c.ahead
    (signal = world[[c.x, c.y]]) and c.turn(signal)
    collide = ->(o) { [o.x, o.y] == [c.x, c.y] }
    if carts.count(&collide) > 1
      collisions.push [c.x, c.y]
      carts.delete_if(&collide)
    end
  end
end

p collisions[0]
p carts
