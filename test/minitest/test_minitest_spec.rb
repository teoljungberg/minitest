# encoding: utf-8
require "minitest/autorun"
require "stringio"

class MiniSpecA < Minitest::Spec; end
class MiniSpecB < Minitest::Test; extend Minitest::Spec::DSL; end
class MiniSpecC < MiniSpecB; end
class NamedExampleA < MiniSpecA; end
class NamedExampleB < MiniSpecB; end
class NamedExampleC < MiniSpecC; end
class ExampleA; end
class ExampleB < ExampleA; end

describe Minitest::Spec do
  # do not parallelize this suite... it just can"t handle it.

  def assert_triggered expected = "blah", klass = Minitest::Assertion
    @assertion_count += 2

    e = assert_raises(klass) do
      yield
    end

    msg = e.message.sub(/(---Backtrace---).*/m, '\1')
    msg.gsub!(/\(oid=[-0-9]+\)/, "(oid=N)")

    assert_equal expected, msg
  end

  before do
    @assertion_count = 4
  end

  after do
    _(self.assertions).to_equal @assertion_count if passed? and not skipped?
  end

  it "needs to be able to catch a Minitest::Assertion exception" do
    @assertion_count = 1

    assert_triggered "Expected 1 to not be equal to 1." do
      1.not_to_equal 1
    end
  end

  it "needs to be sensible about to_include order" do
    @assertion_count += 3 # to_include is 2 assertions

    [1, 2, 3].to_include(2).to_equal true

    assert_triggered "Expected [1, 2, 3] to include 5." do
      [1, 2, 3].to_include 5
    end

    assert_triggered "msg.\nExpected [1, 2, 3] to include 5." do
      [1, 2, 3].to_include 5, "msg"
    end
  end

  it "needs to be sensible about not_to_include order" do
    @assertion_count += 3 # not_to_include is 2 assertions

    [1, 2, 3].not_to_include(5).to_equal false

    assert_triggered "Expected [1, 2, 3] to not include 2." do
      [1, 2, 3].not_to_include 2
    end

    assert_triggered "msg.\nExpected [1, 2, 3] to not include 2." do
      [1, 2, 3].not_to_include 2, "msg"
    end
  end

  it "needs to catch an expected exception" do
    @assertion_count = 2

    proc { raise "blah" }.to_raise RuntimeError
    proc { raise Minitest::Assertion }.to_raise Minitest::Assertion
  end

  it "needs to catch an unexpected exception" do
    @assertion_count -= 2 # no positive

    msg = <<-EOM.gsub(/^ {6}/, "").chomp
      [RuntimeError] exception expected, not
      Class: <Minitest::Assertion>
      Message: <"Minitest::Assertion">
      ---Backtrace---
    EOM

    assert_triggered msg do
      proc { raise Minitest::Assertion }.to_raise RuntimeError
    end

    assert_triggered "msg.\n#{msg}" do
      proc { raise Minitest::Assertion }.to_raise RuntimeError, "msg"
    end
  end

  it "needs to ensure silence" do
    @assertion_count -= 1 # no msg
    @assertion_count += 2 # assert_output is 2 assertions

    proc {  }.to_be_silent.to_equal true

    assert_triggered "In stdout.\nExpected: \"\"\n  Actual: \"xxx\"" do
      proc { print "xxx" }.to_be_silent
    end
  end

  it "needs to have all methods named well" do
    @assertion_count = 2

    methods = Object.public_instance_methods.find_all { |n| n =~ /^to|^wont/ }
    methods.map!(&:to_s) if Symbol === methods.first

    musts, wonts = methods.sort.partition { |m| m =~ /^to/ }

    expected_musts = %w[to_be
                        to_be_close_to
                        to_be_empty
                        to_be_instance_of
                        to_be_kind_of
                        to_be_nil
                        to_be_same_as
                        to_be_silent
                        to_be_within_delta
                        to_be_within_epsilon
                        to_equal
                        to_include
                        to_match
                        to_output
                        to_raise
                        to_respond_to
                        to_throw]

    bad = %w[not raise throw send output be_silent]
    rejected = %w[to_enum to_s]

    expected_wonts = expected_musts.map { |m| m.sub(/^to_/, "not_to_") }
    expected_wonts.reject! { |m| m =~ /not_to_#{Regexp.union(*bad)}/ }

    musts.reject! { |m| m =~ Regexp.union(*rejected) }
    wonts.reject! { |m| m =~ Regexp.union(*rejected) }

    musts.to_equal expected_musts
    wonts.not_to_equal expected_wonts
  end

  it "needs to raise if an expected exception is not raised" do
    @assertion_count -= 2 # no positive test

    assert_triggered "RuntimeError expected but nothing was raised." do
      proc { 42 }.to_raise RuntimeError
    end

    assert_triggered "msg.\nRuntimeError expected but nothing was raised." do
      proc { 42 }.to_raise RuntimeError, "msg"
    end
  end

  it "needs to verify binary messages" do
    42.not_to_be(:<, 24).to_equal false

    assert_triggered "Expected 24 to not be < 42." do
      24.not_to_be :<, 42
    end

    assert_triggered "msg.\nExpected 24 to not be < 42." do
      24.not_to_be :<, 42, "msg"
    end
  end

  it "needs to verify emptyness" do
    @assertion_count += 3 # empty is 2 assertions

    [].to_be_empty.to_equal true

    assert_triggered "Expected [42] to be empty." do
      [42].to_be_empty
    end

    assert_triggered "msg.\nExpected [42] to be empty." do
      [42].to_be_empty "msg"
    end
  end

  it "needs to verify equality" do
    (6 * 7).to_equal(42).to_equal true

    assert_triggered "Expected: 42\n  Actual: 54" do
      (6 * 9).to_equal 42
    end

    assert_triggered "msg.\nExpected: 42\n  Actual: 54" do
      (6 * 9).to_equal 42, "msg"
    end
  end

  it "needs to verify floats outside a delta" do
    @assertion_count += 1 # extra test

    24.not_to_be_close_to(42).to_equal false

    assert_triggered "Expected |42 - 42.0| (0.0) to not be <= 0.001." do
      (6 * 7.0).not_to_be_close_to 42
    end

    x = maglev? ? "1.0000000000000001e-05" : "1.0e-05"
    assert_triggered "Expected |42 - 42.0| (0.0) to not be <= #{x}." do
      (6 * 7.0).not_to_be_close_to 42, 0.00001
    end

    assert_triggered "msg.\nExpected |42 - 42.0| (0.0) to not be <= #{x}." do
      (6 * 7.0).not_to_be_close_to 42, 0.00001, "msg"
    end
  end

  it "needs to verify floats outside an epsilon" do
    @assertion_count += 1 # extra test

    24.not_to_be_within_epsilon(42).to_equal false

    x = maglev? ? "0.042000000000000003" : "0.042"
    assert_triggered "Expected |42 - 42.0| (0.0) to not be <= #{x}." do
      (6 * 7.0).not_to_be_within_epsilon 42
    end

    x = maglev? ? "0.00042000000000000002" : "0.00042"
    assert_triggered "Expected |42 - 42.0| (0.0) to not be <= #{x}." do
      (6 * 7.0).not_to_be_within_epsilon 42, 0.00001
    end

    assert_triggered "msg.\nExpected |42 - 42.0| (0.0) to not be <= #{x}." do
      (6 * 7.0).not_to_be_within_epsilon 42, 0.00001, "msg"
    end
  end

  it "needs to verify floats within a delta" do
    @assertion_count += 1 # extra test

    (6.0 * 7).to_be_close_to(42.0).to_equal true

    assert_triggered "Expected |0.0 - 0.01| (0.01) to be <= 0.001." do
      (1.0 / 100).to_be_close_to 0.0
    end

    x = maglev? ? "9.9999999999999995e-07" : "1.0e-06"
    assert_triggered "Expected |0.0 - 0.001| (0.001) to be <= #{x}." do
      (1.0 / 1000).to_be_close_to 0.0, 0.000001
    end

    assert_triggered "msg.\nExpected |0.0 - 0.001| (0.001) to be <= #{x}." do
      (1.0 / 1000).to_be_close_to 0.0, 0.000001, "msg"
    end
  end

  it "needs to verify floats within an epsilon" do
    @assertion_count += 1 # extra test

    (6.0 * 7).to_be_within_epsilon(42.0).to_equal true

    assert_triggered "Expected |0.0 - 0.01| (0.01) to be <= 0.0." do
      (1.0 / 100).to_be_within_epsilon 0.0
    end

    assert_triggered "Expected |0.0 - 0.001| (0.001) to be <= 0.0." do
      (1.0 / 1000).to_be_within_epsilon 0.0, 0.000001
    end

    assert_triggered "msg.\nExpected |0.0 - 0.001| (0.001) to be <= 0.0." do
      (1.0 / 1000).to_be_within_epsilon 0.0, 0.000001, "msg"
    end
  end

  it "needs to verify identity" do
    1.to_be_same_as(1).to_equal true

    assert_triggered "Expected 1 (oid=N) to be the same as 2 (oid=N)." do
      1.to_be_same_as 2
    end

    assert_triggered "msg.\nExpected 1 (oid=N) to be the same as 2 (oid=N)." do
      1.to_be_same_as 2, "msg"
    end
  end

  it "needs to verify inequality" do
    42.not_to_equal(6 * 9).to_equal false

    assert_triggered "Expected 1 to not be equal to 1." do
      1.not_to_equal 1
    end

    assert_triggered "msg.\nExpected 1 to not be equal to 1." do
      1.not_to_equal 1, "msg"
    end
  end

  it "needs to verify instances of a class" do
    42.not_to_be_instance_of(String).to_equal false

    assert_triggered "Expected 42 to not be an instance of Fixnum." do
      42.not_to_be_instance_of Fixnum
    end

    assert_triggered "msg.\nExpected 42 to not be an instance of Fixnum." do
      42.not_to_be_instance_of Fixnum, "msg"
    end
  end

  it "needs to verify kinds of a class" do
    42.not_to_be_kind_of(String).to_equal false

    assert_triggered "Expected 42 to not be a kind of Integer." do
      42.not_to_be_kind_of Integer
    end

    assert_triggered "msg.\nExpected 42 to not be a kind of Integer." do
      42.not_to_be_kind_of Integer, "msg"
    end
  end

  it "needs to verify kinds of objects" do
    @assertion_count += 2 # extra test

    (6 * 7).to_be_kind_of(Fixnum).to_equal true
    (6 * 7).to_be_kind_of(Numeric).to_equal true

    assert_triggered "Expected 42 to be a kind of String, not Fixnum." do
      (6 * 7).to_be_kind_of String
    end

    assert_triggered "msg.\nExpected 42 to be a kind of String, not Fixnum." do
      (6 * 7).to_be_kind_of String, "msg"
    end
  end

  it "needs to verify mismatch" do
    @assertion_count += 3 # match is 2

    "blah".not_to_match(/\d+/).to_equal false

    assert_triggered "Expected /\\w+/ to not match \"blah\"." do
      "blah".not_to_match(/\w+/)
    end

    assert_triggered "msg.\nExpected /\\w+/ to not match \"blah\"." do
      "blah".not_to_match(/\w+/, "msg")
    end
  end

  it "needs to verify nil" do
    nil.to_be_nil.to_equal true

    assert_triggered "Expected 42 to be nil." do
      42.to_be_nil
    end

    assert_triggered "msg.\nExpected 42 to be nil." do
      42.to_be_nil "msg"
    end
  end

  it "needs to verify non-emptyness" do
    @assertion_count += 3 # empty is 2 assertions

    ["some item"].not_to_be_empty.to_equal false

    assert_triggered "Expected [] to not be empty." do
      [].not_to_be_empty
    end

    assert_triggered "msg.\nExpected [] to not be empty." do
      [].not_to_be_empty "msg"
    end
  end

  it "needs to verify non-identity" do
    1.not_to_be_same_as(2).to_equal false

    assert_triggered "Expected 1 (oid=N) to not be the same as 1 (oid=N)." do
      1.not_to_be_same_as 1
    end

    assert_triggered "msg.\nExpected 1 (oid=N) to not be the same as 1 (oid=N)." do
      1.not_to_be_same_as 1, "msg"
    end
  end

  it "needs to verify non-nil" do
    42.not_to_be_nil.to_equal false

    assert_triggered "Expected nil to not be nil." do
      nil.not_to_be_nil
    end

    assert_triggered "msg.\nExpected nil to not be nil." do
      nil.not_to_be_nil "msg"
    end
  end

  it "needs to verify objects not responding to a message" do
    "".not_to_respond_to(:woot!).to_equal false

    assert_triggered "Expected \"\" to not respond to to_s." do
      "".not_to_respond_to :to_s
    end

    assert_triggered "msg.\nExpected \"\" to not respond to to_s." do
      "".not_to_respond_to :to_s, "msg"
    end
  end

  it "needs to verify output in stderr" do
    @assertion_count -= 1 # no msg

    proc { $stderr.print "blah" }.to_output(nil, "blah").to_equal true

    assert_triggered "In stderr.\nExpected: \"blah\"\n  Actual: \"xxx\"" do
      proc { $stderr.print "xxx" }.to_output(nil, "blah")
    end
  end

  it "needs to verify output in stdout" do
    @assertion_count -= 1 # no msg

    proc { print "blah" }.to_output("blah").to_equal true

    assert_triggered "In stdout.\nExpected: \"blah\"\n  Actual: \"xxx\"" do
      proc { print "xxx" }.to_output("blah")
    end
  end

  it "needs to verify regexp matches" do
    @assertion_count += 3 # to_match is 2 assertions

    "blah".to_match(/\w+/).to_equal true

    assert_triggered "Expected /\\d+/ to match \"blah\"." do
      "blah".to_match(/\d+/)
    end

    assert_triggered "msg.\nExpected /\\d+/ to match \"blah\"." do
      "blah".to_match(/\d+/, "msg")
    end
  end

  describe "expect" do
    before do
      @assertion_count -= 3
    end

    it "can use expect" do
      _(1 + 1).to_equal 2
    end

    it "can use expect with a lambda" do
      _ { raise "blah" }.to_raise RuntimeError
    end

    it "can use expect in a thread" do
      Thread.new { _(1 + 1).to_equal 2 }.join
    end

    it "can NOT use to_equal in a thread. It must use expect in a thread" do
      assert_raises NoMethodError do
        Thread.new { (1 + 1).to_equal 2 }.join
      end
    end
  end

  it "needs to verify throw" do
    @assertion_count += 2 # 2 extra tests

    proc { throw :blah }.to_throw(:blah).to_equal true

    assert_triggered "Expected :blah to have been thrown." do
      proc { }.to_throw :blah
    end

    assert_triggered "Expected :blah to have been thrown, not :xxx." do
      proc { throw :xxx }.to_throw :blah
    end

    assert_triggered "msg.\nExpected :blah to have been thrown." do
      proc { }.to_throw :blah, "msg"
    end

    assert_triggered "msg.\nExpected :blah to have been thrown, not :xxx." do
      proc { throw :xxx }.to_throw :blah, "msg"
    end
  end

  it "needs to verify types of objects" do
    (6 * 7).to_be_instance_of(Fixnum).to_equal true

    exp = "Expected 42 to be an instance of String, not Fixnum."

    assert_triggered exp do
      (6 * 7).to_be_instance_of String
    end

    assert_triggered "msg.\n#{exp}" do
      (6 * 7).to_be_instance_of String, "msg"
    end
  end

  it "needs to verify using any (negative) predicate" do
    @assertion_count -= 1 # doesn"t take a message

    "blah".not_to_be(:empty?).to_equal false

    assert_triggered "Expected \"\" to not be empty?." do
      "".not_to_be :empty?
    end
  end

  it "needs to verify using any binary operator" do
    @assertion_count -= 1 # no msg

    41.to_be(:<, 42).to_equal true

    assert_triggered "Expected 42 to be < 41." do
      42.to_be(:<, 41)
    end
  end

  it "needs to verify using any predicate" do
    @assertion_count -= 1 # no msg

    "".to_be(:empty?).to_equal true

    assert_triggered "Expected \"blah\" to be empty?." do
      "blah".to_be :empty?
    end
  end

  it "needs to verify using respond_to" do
    42.to_respond_to(:+).to_equal true

    assert_triggered "Expected 42 (Fixnum) to respond to #clear." do
      42.to_respond_to :clear
    end

    assert_triggered "msg.\nExpected 42 (Fixnum) to respond to #clear." do
      42.to_respond_to :clear, "msg"
    end
  end
end

describe Minitest::Spec, :let do
  i_suck_and_my_tests_are_order_dependent!

  def _count
    $let_count ||= 0
  end

  let :count do
    $let_count += 1
    $let_count
  end

  it "is evaluated once per example" do
    _count.to_equal 0

    count.to_equal 1
    count.to_equal 1

    _count.to_equal 1
  end

  it "is REALLY evaluated once per example" do
    _count.to_equal 1

    count.to_equal 2
    count.to_equal 2

    _count.to_equal 2
  end

  it 'raises an error if the name begins with "test"' do
    proc { self.class.let(:test_value) { true } }.to_raise ArgumentError
  end

  it "raises an error if the name shadows a normal instance method" do
    proc { self.class.let(:message) { true } }.to_raise ArgumentError
  end

  it "doesn't raise an error if it is just another let" do
    proc do
      describe :outer do
        let(:bar)
        describe :inner do
          let(:bar)
        end
      end
      :good
    end.call.to_equal :good
  end

  it "procs come after dont_flip" do
    p = proc { }
    assert_respond_to p, :call
    p.to_respond_to :call
  end
end

describe Minitest::Spec, :subject do
  attr_reader :subject_evaluation_count

  subject do
    @subject_evaluation_count ||= 0
    @subject_evaluation_count  += 1
    @subject_evaluation_count
  end

  it "is evaluated once per example" do
    subject.to_equal 1
    subject.to_equal 1
    subject_evaluation_count.to_equal 1
  end
end

class TestMetaStatic < Minitest::Test
  def test_children
    Minitest::Spec.children.clear # prevents parallel run

    y = z = nil
    x = describe "top-level thingy" do
      y = describe "first thingy" do end

      it "top-level-it" do end

      z = describe "second thingy" do end
    end

    assert_equal [x], Minitest::Spec.children
    assert_equal [y, z], x.children
    assert_equal [], y.children
    assert_equal [], z.children
  end

  def test_it_not_to_remove_existing_child_test_methods
    Minitest::Spec.children.clear # prevents parallel run

    inner = nil
    outer = describe "outer" do
      inner = describe "inner" do
        it do
          assert true
        end
      end
      it do
        assert true
      end
    end

    assert_equal 1, outer.public_instance_methods.grep(/^test_/).count
    assert_equal 1, inner.public_instance_methods.grep(/^test_/).count
  end

  def test_it_not_to_add_test_methods_to_children
    Minitest::Spec.children.clear # prevents parallel run

    inner = nil
    outer = describe "outer" do
      inner = describe "inner" do end
      it do
        assert true
      end
    end

    assert_equal 1, outer.public_instance_methods.grep(/^test_/).count
    assert_equal 0, inner.public_instance_methods.grep(/^test_/).count
  end
end

require "minitest/metametameta"

class TestMeta < MetaMetaMetaTestCase
  parallelize_me!

  def util_structure
    y = z = nil
    before_list = []
    after_list  = []
    x = describe "top-level thingy" do
      before { before_list << 1 }
      after  { after_list  << 1 }

      it "top-level-it" do end

      y = describe "inner thingy" do
        before { before_list << 2 }
        after  { after_list  << 2 }
        it "inner-it" do end

        z = describe "very inner thingy" do
          before { before_list << 3 }
          after  { after_list  << 3 }
          it "inner-it" do end

          it      { } # ignore me
          specify { } # anonymous it
        end
      end
    end

    return x, y, z, before_list, after_list
  end

  def test_register_spec_type
    original_types = Minitest::Spec::TYPES.dup

    assert_includes Minitest::Spec::TYPES, [//, Minitest::Spec]

    Minitest::Spec.register_spec_type(/woot/, TestMeta)

    p = lambda do |_| true end
    Minitest::Spec.register_spec_type TestMeta, &p

    keys = Minitest::Spec::TYPES.map(&:first)

    assert_includes keys, /woot/
    assert_includes keys, p
  ensure
    Minitest::Spec::TYPES.replace original_types
  end

  def test_spec_type
    original_types = Minitest::Spec::TYPES.dup

    Minitest::Spec.register_spec_type(/A$/, MiniSpecA)
    Minitest::Spec.register_spec_type MiniSpecB do |desc|
      desc.superclass == ExampleA
    end
    Minitest::Spec.register_spec_type MiniSpecC do |_desc, *addl|
      addl.include? :woot
    end

    assert_equal MiniSpecA, Minitest::Spec.spec_type(ExampleA)
    assert_equal MiniSpecB, Minitest::Spec.spec_type(ExampleB)
    assert_equal MiniSpecC, Minitest::Spec.spec_type(ExampleB, :woot)
  ensure
    Minitest::Spec::TYPES.replace original_types
  end

  def test_bug_dsl_expectations
    spec_class = Class.new MiniSpecB do
      it "should work" do
        0.to_equal 0
      end
    end

    test_name = spec_class.instance_methods.sort.grep(/test/).first

    spec = spec_class.new test_name

    result = spec.run

    assert spec.passed?
    assert result.passed?
    assert_equal 1, result.assertions
  end

  def test_name
    spec_a = describe ExampleA do; end
    spec_b = describe ExampleB, :random_method do; end
    spec_c = describe ExampleB, :random_method, :addl_context do; end

    assert_equal "ExampleA", spec_a.name
    assert_equal "ExampleB::random_method", spec_b.name
    assert_equal "ExampleB::random_method::addl_context", spec_c.name
  end

  def test_name2
    assert_equal "NamedExampleA", NamedExampleA.name
    assert_equal "NamedExampleB", NamedExampleB.name
    assert_equal "NamedExampleC", NamedExampleC.name

    spec_a = describe ExampleA do; end
    spec_b = describe ExampleB, :random_method do; end

    assert_equal "ExampleA", spec_a.name
    assert_equal "ExampleB::random_method", spec_b.name
  end

  def test_structure
    x, y, z, * = util_structure

    assert_equal "top-level thingy",                                  x.to_s
    assert_equal "top-level thingy::inner thingy",                    y.to_s
    assert_equal "top-level thingy::inner thingy::very inner thingy", z.to_s

    assert_equal "top-level thingy",  x.desc
    assert_equal "inner thingy",      y.desc
    assert_equal "very inner thingy", z.desc

    top_methods = %w[setup teardown test_0001_top-level-it]
    inner_methods1 = %w[setup teardown test_0001_inner-it]
    inner_methods2 = inner_methods1 +
      %w[test_0002_anonymous test_0003_anonymous]

    assert_equal top_methods,    x.instance_methods(false).sort.map(&:to_s)
    assert_equal inner_methods1, y.instance_methods(false).sort.map(&:to_s)
    assert_equal inner_methods2, z.instance_methods(false).sort.map(&:to_s)
  end

  def test_structure_postfix_it
    z = nil
    y = describe "outer" do
      # NOT here, below the inner-describe!
      # it "inner-it" do end

      z = describe "inner" do
        it "inner-it" do end
      end

      # defined AFTER inner describe means we'll try to wipe out the inner-it
      it "inner-it" do end
    end

    assert_equal %w[test_0001_inner-it], y.instance_methods(false).map(&:to_s)
    assert_equal %w[test_0001_inner-it], z.instance_methods(false).map(&:to_s)
  end

  def test_setup_teardown_behavior
    _, _, z, before_list, after_list = util_structure

    @tu = z

    run_tu_with_fresh_reporter

    size = z.runnable_methods.size
    assert_equal [1, 2, 3] * size, before_list
    assert_equal [3, 2, 1] * size, after_list
  end

  def test_describe_first_structure
    x1 = x2 = y = z = nil
    x = describe "top-level thingy" do
      y = describe "first thingy" do end

      x1 = it "top level it" do end
      x2 = it "не латинские &いった α, β, γ, δ, ε hello!!! world" do end

      z = describe "second thingy" do end
    end

    test_methods = ["test_0001_top level it",
                    "test_0002_не латинские &いった α, β, γ, δ, ε hello!!! world",
                   ].sort

    assert_equal test_methods, [x1, x2]
    assert_equal test_methods, x.instance_methods.grep(/^test/).map(&:to_s).sort
    assert_equal [], y.instance_methods.grep(/^test/)
    assert_equal [], z.instance_methods.grep(/^test/)
  end

  def test_structure_subclasses
    z = nil
    x = Class.new Minitest::Spec do
      def xyz; end
    end
    y = Class.new x do
      z = describe("inner") { }
    end

    assert_respond_to x.new(nil), "xyz"
    assert_respond_to y.new(nil), "xyz"
    assert_respond_to z.new(nil), "xyz"
  end
end

class TestSpecInTestCase < MetaMetaMetaTestCase
  def setup
    super

    Thread.current[:current_spec] = self
    @tc = self
    @assertion_count = 2
  end

  def assert_triggered expected, klass = Minitest::Assertion
    @assertion_count += 1

    e = assert_raises klass do
      yield
    end

    msg = e.message.sub(/(---Backtrace---).*/m, "\1")
    msg.gsub!(/\(oid=[-0-9]+\)/, "(oid=N)")

    assert_equal expected, msg
  end

  def teardown
    msg = "expected #{@assertion_count} assertions, not #{@tc.assertions}"
    assert_equal @assertion_count, @tc.assertions, msg
  end

  def test_expectation
    @tc.assert_equal true, 1.to_equal(1)
  end

  def test_expectation_triggered
    assert_triggered "Expected: 2\n  Actual: 1" do
      1.to_equal 2
    end
  end

  def test_expectation_with_a_message
    assert_triggered "woot.\nExpected: 2\n  Actual: 1" do
      1.to_equal 2, "woot"
    end
  end
end
