## Ruby comment
class Hello
  def print_greeting s
    puts s
  end
end

x = Hello.new
x.print_greeting "Hello, world!"

class A
  def m1
    34
  end

  def m2(x, y)
    z = 7
    if x > 1
      false
    else
      x + y * z
    end
  end
end

class B
  def m1
    4
  end

  def m3 x
    x.abs * 2 + self.m1
  end
end

## State

class E
  def initialize(f=0) # Like constructor
    @foo = f
  end

  def m1
    @foo = 0
  end

  def m2 x
    @foo += x
  end

  def foo
    @foo
  end

  def self.class_name # static methods
    @@ClassName
  end

  def self.set_class_name s
    @@ClassName = s
  end
end

# Attributes: Syntactic Sugar for getters/setters

class Attributes
  attr_accessor :str
  def print
    "The value is: " + @str
  end
end

# All fields are private! Excellent!
# Methods visibility

class Visibility

  def print_public
    "Hello" + print_private
  end

  private
  def print_private
    " World!"
  end
end
