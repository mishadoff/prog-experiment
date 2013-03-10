class Ratio
  attr_accessor :num, :den

  def initialize(num, den=1)
    if den == 0
      raise "Denominator must be non zero"
    elsif den < 0
      @num = -num
      @den = -den
    else
      @num = num
      @den = den
    end
    reduce # Reduce ratio to normal form
  end

  # tostring method
  def to_s
    ans = @num.to_s
    if @den != 1
      ans += "/"
      ans += @den.to_s
    end
    ans
  end

  def to_s2
    dens = ""
    dens = "/" + @den.to_s if @den != 1
    @num.to_s + dens
  end

  def add! r # Mutable addition
    a = r.num
    b = r.den
    c = @num
    d = @den
    @num = (a * d) + (b * c)
    @den = b * d
    reduce
    self
  end

  def + r # Copy addition
    ans = Ratio.new(@num, @den)
    ans.add! r
  end

private
  def gcd(x,y) # Greatest common divisor
    if x == y
      x
    elsif x < y
      gcd(x, y - x)
    else
      gcd(y,x)
    end
  end

  def reduce
    if @num == 0
      @den = 1
    else
      d = gcd(@num.abs, den)
      @num = @num / d
      @den = @den / d
    end
  end
end

def use_rationals
  r1 = Ratio.new(3,4)
  r2 = r1 + r1 + Ratio.new(-5,2)
  puts r2.to_s
  (r2.add! r1).add! (Ratio.new(1, -4))
  puts r2.to_s
  puts r2.to_s2
end
