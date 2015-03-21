##
# It's where you hide your "assertions".
#
# Please note, because of the way that expectations are implemented,
# all expectations (eg to_equal) are dependent upon a thread local
# variable +:current_spec+. If your specs rely on mixing threads into
# the specs themselves, you're better off using assertions or the new
# _(value) wrapper. For example:
#
#     it "should still work in threads" do
#       my_threaded_thingy do
#         (1+1).to_equal 2         # bad
#         assert_equal 2, 1+1        # good
#         _(1 + 1).to_equal 2      # good
#         value(1 + 1).to_equal 2  # good, also #expect
#       end
#     end

module Minitest::Expectations

  ##
  # Returns a value monad that has all of Expectations methods
  # available to it.
  #
  # Also aliased to #value and #expect for your aesthetic pleasure:
  #
  #         _(1 + 1).to_equal 2
  #     value(1 + 1).to_equal 2
  #    expect(1 + 1).to_equal 2
  #
  # This method of expectation-based testing is preferable to
  # straight-expectation methods (on Object) because it stores its
  # test context, bypassing our hacky use of thread-local variables.
  #
  # At some point, the methods on Object will be deprecated and then
  # removed.

  def _ value = nil, &block
    Minitest::Expectation.new block || value, self
  end

  alias value _
  alias expect _

  ##
  # See Minitest::Assertions#assert_empty.
  #
  #    collection.to_be_empty
  #
  # :method: to_be_empty

  infect_an_assertion :assert_empty, :to_be_empty, :unary

  ##
  # See Minitest::Assertions#assert_equal
  #
  #    a.to_equal b
  #
  # :method: to_equal

  infect_an_assertion :assert_equal, :to_equal

  ##
  # See Minitest::Assertions#assert_in_delta
  #
  #    n.to_be_close_to m [, delta]
  #
  # :method: to_be_close_to

  infect_an_assertion :assert_in_delta, :to_be_close_to

  alias :to_be_within_delta :to_be_close_to # :nodoc:

  ##
  # See Minitest::Assertions#assert_in_epsilon
  #
  #    n.to_be_within_epsilon m [, epsilon]
  #
  # :method: to_be_within_epsilon

  infect_an_assertion :assert_in_epsilon, :to_be_within_epsilon

  ##
  # See Minitest::Assertions#assert_includes
  #
  #    collection.to_include obj
  #
  # :method: to_include

  infect_an_assertion :assert_includes, :to_include, :reverse

  ##
  # See Minitest::Assertions#assert_instance_of
  #
  #    obj.to_be_instance_of klass
  #
  # :method: to_be_instance_of

  infect_an_assertion :assert_instance_of, :to_be_instance_of

  ##
  # See Minitest::Assertions#assert_kind_of
  #
  #    obj.to_be_kind_of mod
  #
  # :method: to_be_kind_of

  infect_an_assertion :assert_kind_of, :to_be_kind_of

  ##
  # See Minitest::Assertions#assert_match
  #
  #    a.to_match b
  #
  # :method: to_match

  infect_an_assertion :assert_match, :to_match

  ##
  # See Minitest::Assertions#assert_nil
  #
  #    obj.to_be_nil
  #
  # :method: to_be_nil

  infect_an_assertion :assert_nil, :to_be_nil, :unary

  ##
  # See Minitest::Assertions#assert_operator
  #
  #    n.to_be :<=, 42
  #
  # This can also do predicates:
  #
  #    str.to_be :empty?
  #
  # :method: to_be

  infect_an_assertion :assert_operator, :to_be, :reverse

  ##
  # See Minitest::Assertions#assert_output
  #
  #    proc { ... }.to_output out_or_nil [, err]
  #
  # :method: to_output

  infect_an_assertion :assert_output, :to_output

  ##
  # See Minitest::Assertions#assert_raises
  #
  #    proc { ... }.to_raise exception
  #
  # :method: to_raise

  infect_an_assertion :assert_raises, :to_raise

  ##
  # See Minitest::Assertions#assert_respond_to
  #
  #    obj.to_respond_to msg
  #
  # :method: to_respond_to

  infect_an_assertion :assert_respond_to, :to_respond_to, :reverse

  ##
  # See Minitest::Assertions#assert_same
  #
  #    a.to_be_same_as b
  #
  # :method: to_be_same_as

  infect_an_assertion :assert_same, :to_be_same_as

  ##
  # See Minitest::Assertions#assert_silent
  #
  #    proc { ... }.to_be_silent
  #
  # :method: to_be_silent

  infect_an_assertion :assert_silent, :to_be_silent

  ##
  # See Minitest::Assertions#assert_throws
  #
  #    proc { ... }.to_throw sym
  #
  # :method: to_throw

  infect_an_assertion :assert_throws, :to_throw

  ##
  # See Minitest::Assertions#refute_empty
  #
  #    collection.not_to_be_empty
  #
  # :method: not_to_be_empty

  infect_an_assertion :refute_empty, :not_to_be_empty, :unary

  ##
  # See Minitest::Assertions#refute_equal
  #
  #    a.not_to_equal b
  #
  # :method: not_to_equal

  infect_an_assertion :refute_equal, :not_to_equal

  ##
  # See Minitest::Assertions#refute_in_delta
  #
  #    n.not_to_be_close_to m [, delta]
  #
  # :method: not_to_be_close_to

  infect_an_assertion :refute_in_delta, :not_to_be_close_to

  alias :not_to_be_within_delta :not_to_be_close_to # :nodoc:

  ##
  # See Minitest::Assertions#refute_in_epsilon
  #
  #    n.not_to_be_within_epsilon m [, epsilon]
  #
  # :method: not_to_be_within_epsilon

  infect_an_assertion :refute_in_epsilon, :not_to_be_within_epsilon

  ##
  # See Minitest::Assertions#refute_includes
  #
  #    collection.not_to_include obj
  #
  # :method: not_to_include

  infect_an_assertion :refute_includes, :not_to_include, :reverse

  ##
  # See Minitest::Assertions#refute_instance_of
  #
  #    obj.not_to_be_instance_of klass
  #
  # :method: not_to_be_instance_of

  infect_an_assertion :refute_instance_of, :not_to_be_instance_of

  ##
  # See Minitest::Assertions#refute_kind_of
  #
  #    obj.not_to_be_kind_of mod
  #
  # :method: not_to_be_kind_of

  infect_an_assertion :refute_kind_of, :not_to_be_kind_of

  ##
  # See Minitest::Assertions#refute_match
  #
  #    a.not_to_match b
  #
  # :method: not_to_match

  infect_an_assertion :refute_match, :not_to_match

  ##
  # See Minitest::Assertions#refute_nil
  #
  #    obj.not_to_be_nil
  #
  # :method: not_to_be_nil

  infect_an_assertion :refute_nil, :not_to_be_nil, :unary

  ##
  # See Minitest::Assertions#refute_operator
  #
  #    n.not_to_be :<=, 42
  #
  # This can also do predicates:
  #
  #    str.not_to_be :empty?
  #
  # :method: not_to_be

  infect_an_assertion :refute_operator, :not_to_be, :reverse

  ##
  # See Minitest::Assertions#refute_respond_to
  #
  #    obj.not_to_respond_to msg
  #
  # :method: not_to_respond_to

  infect_an_assertion :refute_respond_to, :not_to_respond_to, :reverse

  ##
  # See Minitest::Assertions#refute_same
  #
  #    a.not_to_be_same_as b
  #
  # :method: not_to_be_same_as

  infect_an_assertion :refute_same, :not_to_be_same_as
end
