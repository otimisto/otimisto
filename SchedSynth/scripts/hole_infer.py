# order
import pulp as p
import json
import argparse

def edit_distance(s1, s2):
    def less_than(e1, e2):
        e1_ind = s2.index(e1)
        e2_ind = s2.index(e2)

        return e1_ind < e2_ind

    # Now bubble sort using this less_than
    # counting the number of swap
    swap_count = 0
    for index in range(len(s1)):
        for j in range(index + 1, len(s1)):
            if less_than(s1[j], s1[index]):
                s1[index], s1[j] = s1[j], s1[index]
                swap_count += 1

    # print("Swapped ", s1, "target", s2)
    return swap_count

# get permutations of the list
def permutations(l):
    if len(l) == 0:
        return []
    if len(l) == 1:
        return [l]
    lst = []
    for i in range(len(l)):
        m = l[i]
        remLst = l[:i] + l[i+1:]
        for p in permutations(remLst):
            lst.append([m] + p)
    return lst

# get all the different ways to split the list l
# into n sublists
def splits(l, n):
    # base case
    if n < 1:
        print("Can't negative split!")
        raise error
    if n == 1:
        return [[l]]
    # recursive case
    else:
        result = []
        for i in range(len(l)):
            # split the list into two parts
            part1 = l[:i]
            part2 = l[i:]
            # recursively split the second part
            for sublist in splits(part2, n-1):
                this_list = []
                result.append([part1] + sublist)
        return result

def fill_holes(l, fills):
    res = []
    fill_index = 0

    for elem in l:
        if not_hole(elem):
            res.append(elem)
        else:
            res += fills[fill_index]
            fill_index += 1
    return res

def print_grid(l, size):
    for i in range(size):
        for j in range(size):
            print(p.value(l[i][j]), end=",")
        print("")

def all_options(input_list, hole_fills):
    # Go through the input_list and find every permutation
    # of holes that can fill the holes in the input list.
    hole_indexes = [i for i in range(len(input_list)) if hole(input_list[i])]
    if len(hole_indexes) == 0:
        print("Nothing to fill")
        return []
    permuted_fills = permutations(hole_fills)
    fills_to_use = []
    for perm in permuted_fills:
        fills_to_use += splits(perm, len(hole_indexes))

    options = []
    for fill in fills_to_use:
        options.append(fill_holes(input_list, fill))

    return options

target = [1,2,3,4,5,6]

def input_generator(n, holes=3):
    import random

    input = [i for i in range(1, n + 1)]
    remainder =  []

    # shuffle, put the holes at the beginning, then
    # shuffle at the end
    random.shuffle(input)
    for i in range(holes):
        remainder.append(input[i])
        input[i] = 'hole'
    random.shuffle(input)

    return input, remainder

def get_variables_from(var_list):
    res = []
    for var in var_list:
        if not_hole(var):
            res.append(var)

    return res

def hole(h):
    return h == 'hole' or h == 'single_hole'

def not_hole(h):
    return not hole(h)

def ilp_insert(input_list, hole_fills, target_list):
    variables_list = sorted(get_variables_from(input_list) + get_variables_from(hole_fills))
    prob = p.LpProblem('Problem', p.LpMinimize)

    # Two types of variables: one for each position/variable pair, that are
    # binary variables.
    # Other type is the index variable for each variable in the variable
    # list that specify the location of the variables.
    location_indicators = []
    position_variables = []

    # map from a variable name to a location in the variables list
    # (and therefore the location and position maps)
    index_map = {}
    vnames = []
    for index, var in enumerate(variables_list):
        index_map[var] = index
        varname = "Var" + str(var)
        var_locations = []
        # compute the indiicators -- vari is 1 if var is placed
        # at position i.
        for i, target_location in enumerate(target_list):
            position_indicator = p.LpVariable(str(varname) + "_" + str(i + 1), lowBound=0, cat='Integer')
            vnames.append(str(varname) + "_" + str(i + 1))
            var_locations.append(position_indicator)

            prob += position_indicator <= 1
            prob += position_indicator >= 0

        location_indicators.append(var_locations)

        # Add the overall constrait that the var can only be in one
        # location.
        prob += (sum(var_locations) == 1)

        # compute
        vnames.append(varname)
        position_var = p.LpVariable(str(varname), lowBound=0, cat='Integer')
        position_variables.append(position_var)
        prob += position_var == sum([(i + 1) * var_locations[i] for i in range(len(var_locations))])

    # Add constraints that every location has at most one variable.
    for j in range(len(variables_list)):
        prob += sum([location_indicators[i][j] for i in range(len(location_indicators))]) == 1

    # Add constraints from the non-hole locations
    last_specified = None
    last_non_none = None

    for i, inp in enumerate(input_list):
        position_var_index = None
        if not_hole(inp):
            position_var_index = index_map[inp]
        # this is the start: tie the input to the start
        if i == 0 and not_hole(inp):
            # get the index of this in the variables_list.
            prob += (location_indicators[position_var_index][0] == 1)
        if i == len(input_list) - 1 and not_hole(inp):
            prob += (position_variables[position_var_index] == len(target_list))

        # If the last one was not a none then need to link these
        # together
        if last_specified is not None and position_var_index is not None:
            prob += ((position_variables[last_specified] + 1) == position_variables[position_var_index])
        elif position_var_index is not None and last_non_none is not None:
            # tie together the vars, but in a looser way that allows
            # for stuff in-between.
            prob += ((position_variables[last_non_none]) <= position_variables[position_var_index])

        last_specified = position_var_index
        if position_var_index is not None:
            last_non_none = position_var_index
        

    # Add constraits around fixed-sized holes.  We look for the following patterns
    # that add constraints. (one ? == fixed sized hole, ?? == variable sized hole)
    # ?, ?, ..., X, ... : (fixed number of holes tied to input) (X == 2)
    # ..., X, ?, ?: (fixed number of holes tied to output) (X == len - 2)
    # X, ?, ?, Y: (fixed number of holes between variables (X == Y - 2)
    # X, ?, ?, ??, Y: (fixed number of holes between variables with additional unfixed
    # holes. (X <= Y - 2)
    fixed_count_from_input = 0 # first case
    fixed_count_to_output = 0 # second case
    fixed_count_between_variables = 0 # third case
    minimim_count_between_variables = 0 # fourth case
    last_fixed_variable = None
    for i, inp in enumerate(input_list):
        if not_hole(inp):
            position_index = index_map[inp]
        else:
            position_index = None

        if inp == 'hole':
            fixed_count_from_input = None
            fixed_count_to_output = None
            fixed_count_between_variables = None
        elif inp == 'single_hole':
            if fixed_count_to_output is None:
                fixed_count_to_output = 1
            else:
                fixed_count_to_output += 1
            if fixed_count_from_input is not None:
                fixed_count_from_input += 1
            if fixed_count_between_variables is not None:
                fixed_count_between_variables += 1
            minimim_count_between_variables += 1
        else:
            # this is a variable.  check the counters; if nessecary add
            # constraints as appropriate. and reset the counters.
            if fixed_count_from_input is not None:
                # fix this the right distance from the start
                prob += (location_indicators[position_index][fixed_count_from_input] == 1)
            if fixed_count_between_variables is not None and last_fixed_variable is not None and fixed_count_between_variables > 0:
                # need to add one to account for this variable (otherwise we are just
                # putting things next to each other)
                prob += (position_variables[last_fixed_variable] == (position_variables[position_index] - (fixed_count_between_variables + 1)) )
            elif minimim_count_between_variables is not None and last_fixed_variable is not None and minimim_count_between_variables > 0:
                # only specify the minimum count if the fixed count
                # isn't set.
                prob += ( position_variables[last_fixed_variable] <= (position_variables[position_index] - (minimim_count_between_variables + 1)) )

            # reset counters
            last_fixed_variable = position_index
            fixed_count_from_input = None # no longer needed after first var
            fixed_count_to_output = 0 #reset this
            fixed_count_between_variables = 0 #reset this
            minimim_count_between_variables = 0 # reset this

    if fixed_count_to_output is not None and last_fixed_variable is not None and fixed_count_to_output > 0:
        # Fix this variable to the output
        # Don't + 1 here because there's a 
        prob += (position_variables[last_fixed_variable] == (len(target) - 1) - fixed_count_to_output)

    # Variables for the overall score from locations in the target list
    score_variables = []
    for i, variable in enumerate(target_list):
        # Get the index of the variable in the array.
        list_index = index_map[variable]

        this_position_variable = position_variables[list_index]
        position_variable_difference = p.LpVariable("Var" + str(variable) + "_Difference", cat='Integer')
        prob += position_variable_difference == ((i + 1) - this_position_variable)

        score_variables.append(position_variable_difference)

    # We need to optimize wrt. the absolute value, so need to do this odd hack to do
    # that.
    u_variables = []
    for var in score_variables:
        u_var = p.LpVariable("UVar_" + str(var), cat='Integer')
        prob += -u_var <= var
        prob += var <= u_var
        u_variables.append(u_var)

    # Need to set up these indicators -- they allow the multiplications below.
    ## These indiciators start at 1, and decrease to 0 when the variable
    ## has been set.

    ### basically, this is (last_cell - location_indicator)
    decreasing_indicators = []
    ## This is the opposite --- set to 1 - decerasing_indicators.
    increasing_indicators = []
    for i, var in enumerate(variables_list):
        dec_indicators = []
        inc_indicators = []
        last_var = 1
        for j in range(len(variables_list)):
            dec_indicator = p.LpVariable("Var" + str(var) + "_DecreasingIndicator" + str(j), cat='Integer')
            inc_indicator = p.LpVariable("Var" + str(var) + "_IncreasingIndicator" + str(j), cat='Integer')
            prob += (dec_indicator == (last_var - location_indicators[i][j]))
            prob += (inc_indicator == (1 - dec_indicator))
            last_var = dec_indicator # keep track of de indicators

            dec_indicators.append(dec_indicator)
            inc_indicators.append(inc_indicator)

        decreasing_indicators.append(dec_indicators)
        increasing_indicators.append(inc_indicators)

    # Work out the number of swaps required: this is one swap for every inverted-pair.
    # We use the indicator variables for this: summing 
    # everything 'below' and 'to the left' of each variable
    # (that's the things that are 'greater' and 'in the wrong place')
    swaps = []
    individual_swap_count = []
    # tracker variable for debugging
    for i, var in enumerate(position_variables):
        # see if this variable has any out-of-position
        swap_total_variable = p.LpVariable("Var" + str(var) + "_SwapCount", cat='Integer')
        swap_count_list = []

        # j iterates over vars
        for j in range(len(location_indicators)):
            # k over positions
            for k in range(len(location_indicators[j])):
                swap_var_indicator = p.LpVariable("Var" + str(var) + "_SwapAt_" + str(j) + "-" + str(k), cat='Integer')
                # needed if both location_indicator and the inc/dec are 0
                prob += swap_var_indicator >= 0

                # Use big-M approach--- but M == 1 is what we need here since
                # both the indicator variables are either 0 or 1
                if j > i:
                    prob += ( swap_var_indicator <= location_indicators[j][k] )
                    prob += ( swap_var_indicator <= decreasing_indicators[i][k] )
                    prob += ( swap_var_indicator >= location_indicators[j][k] - (1 - decreasing_indicators[i][k]) )
                if j < i:
                    prob += ( swap_var_indicator <= location_indicators[j][k] )
                    prob += ( swap_var_indicator <= increasing_indicators[i][k] )
                    prob += ( swap_var_indicator >= location_indicators[j][k] - (1 - increasing_indicators[i][k]) )

                swap_count_list.append(swap_var_indicator)

        prob += swap_total_variable == sum(swap_count_list)
        swaps.append(swap_total_variable)
        individual_swap_count.append(swap_count_list)

    # Objective to minimize this
    # prob += sum(u_variables) ## this one minimizes average distance, but produces
    ## some odd results
    prob += sum(swaps) ## minimize the number of swaps
    # Get the location:
    status = prob.solve()
    # prob.toJson('output.json')
    # with open('output.json', 'r') as f:
    #     j = json.load(f)
    # with open('output.json', 'w') as f:
    #     f.write(json.dumps(j, indent=1))
    result = [None] * len(position_variables)
    # for i, position in enumerate(swaps):
    #     print("variable " +str(i + 1) + " has seaps:", p.value(position))
    for i, position in enumerate(position_variables):
        print("variable " +str(i + 1) + " has location:", p.value(position))
        result[int(p.value(position)) - 1] = i + 1

    # for i in range(len(location_indicators)):
    #     print("Var " + str(i + 1) + ":", end="")
    #     for j in range(len(location_indicators)):
    #         print(p.value(location_indicators[i][j]), end=", ")
    #     print("")
    return result


def in_order_insert(input_list, hole_fills, target_list):
    # Put the hole_fills in the order of the target_list.
    ordered_hole_fills = []
    fill_locations = []
    for i, target in enumerate(target_list):
        if target in hole_fills:
            ordered_hole_fills.append(target)
            fill_locations.append(i)

    # Now, put everything in the hole closest to it.
    hole_locations = []
    for i, inp in enumerate(input_list):
        if hole(inp):
            hole_locations.append(i)
    hole_locations.append(10000000) # extra number on the end to 
    # make rnage checks below simpler.

    # Put everything in the hole nearest the hole location
    # we're interested in.
    hole_index = 0
    final_fills = []
    fills_in_this_hole = []
    for i, fill in enumerate(fill_locations):
        # relative distances of this hole from the final location
        dist_this = abs(hole_locations[hole_index] - fill)
        dist_next = abs(hole_locations[hole_index + 1] - fill)

        # relative distances of the elements in the hole that
        # will be pushed closer or further from their final location.

        if dist_this < dist_next:
            fills_in_this_hole.append(ordered_hole_fills[i])
        else:
            final_fills.append(fills_in_this_hole)
            fills_in_this_hole = [ordered_hole_fills[i]]
            hole_index += 1

    final_fills.append(fills_in_this_hole)
    while len(final_fills) < len(hole_locations) - 1:
        final_fills.append([])

    return final_fills

# aim of this algorithm is like a partial bubble-sort
# Example where this doesn't work is For input  [None, 4, None, 2, 1, None]
# gets stuck on [3, 4, 2, 1, 5, 6] (local maximum)
def swap_alg(inputs, fills, target):
    print("Looking at ", inputs)
    def join(blocks):
        res = []
        for b in blocks:
            for elt in b:
                if not_hole(elt) and elt is not None:
                    res.append(elt)
        return res

    # block this list differently.
    # the aim is to put things together
    # in blocks that cannot be shuffled internally.
    blocked_inputs = []
    block = []
    fills_added = False
    for inp in inputs:
        if hole(inp):
            # if we have a block, put that in.
            if len(block) > 0:
                blocked_inputs.append(block + [None])
                block = []
            # put all the fills required in the first spot for them.
            if not fills_added:
                for fill in fills:
                    blocked_inputs.append([fill])
                fills_added = True
        else: # inp is not none --- create a block with this elt.
            block.append(inp)
    if len(block) > 0:
        blocked_inputs.append(block + [None])

    tied_at_start = not_hole(inputs[0])
    tied_at_end = not_hole(inputs[-1])

    def can_swap(i, j):
        if i >= len(blocked_inputs) or j >= len(blocked_inputs):
            return False
        if len(blocked_inputs[i]) > 1 and len(blocked_inputs[j]) > 1:
            # these are groups of variables
            return False
        if i == 0 and tied_at_start:
            return False
        if j == len(blocked_inputs) - 1 and tied_at_end:
            return False
        return True
        

    # now, do bubble-sort while things change
    changed = True
    print("Initial" + str(join(blocked_inputs)))
    while changed:
        changed = False
        for i in range(0, len(blocked_inputs)):
            if len(blocked_inputs[i]) > 1:
                print("Skipped ", blocked_inputs[i])
                continue
            for j in range(i + 1, len(blocked_inputs)):
                if len(blocked_inputs[j]) > 1 and j != i + 1:
                    continue
                current_score = edit_distance(join(blocked_inputs), target)
                if can_swap(i, j):
                    blocked_inputs[i], blocked_inputs[j] = blocked_inputs[j], blocked_inputs[i]
                    next_score = edit_distance(join(blocked_inputs), target)

                    if next_score < current_score:
                        print("Changed (", current_score, next_score, ")")
                        print("Changed (", blocked_inputs[i], blocked_inputs[j], ")")
                        print(join(blocked_inputs))
                        changed = True
                    else:
                        # unswap
                        blocked_inputs[i], blocked_inputs[j] = blocked_inputs[j], blocked_inputs[i]
    return join(blocked_inputs)


def parse_inputs(input_string, num_elements):
    x = input_string.split(',')
    input_list = []

    for elt in x:
        if elt == 'hole':
            loop_num = 'hole'
        elif elt == 'single_hole':
            loop_num = 'single_hole'
        elif elt:
            # parse into number
            loop_num = int(elt)
        else:
            print("Warning, unexpected loop elt ", elt)
            raise Error()
        input_list.append(loop_num)

    target_list = list(range(1, num_elements + 1))

    # calculate the hole fills, which are all the elemtns in one list but not
    # the other.
    hole_fills = []
    for elt in target_list:
        if elt not in input_list:
            hole_fills.append(elt)

    return input_list, hole_fills, target_list

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--test", default=False, action='store_true') # compare all the different strategies
    parser.add_argument("--size-test", default=None, dest='size_test', type=int) # just run problems of size X
    parser.add_argument("--size-test-holes", default=3, dest='size_test_holes', type=int) # number of holes to use when size testing

    parser.add_argument('target_order', help='input order, comma separated. It is assumed that the input is just a string of numbers increasing in size.  This should be comman-separated. Holes are indicated by "hole" or "single_hole" (to specify variable size holes or individual sized holes)')
    parser.add_argument('max_element', help='how many loop nests to include in the tareget list', type=int)

    args = parser.parse_args()

    if args.size_test is None and not args.test:
        # Run normally
        input_list, hole_fills, target = parse_inputs(args.target_order, args.max_element)
        print("Running for ", input_list)
        print("With hole fills ", hole_fills)

        # run ilp solver
        result = ilp_insert(input_list, hole_fills, target)
        print("Result:", result)
        ilp_score = edit_distance(result[:], target)
        print("ILP Score is (lower is better) ", ilp_score)

    if args.size_test is not None:
        input_list, hole_fills = input_generator(args.size_test, holes=args.size_test_holes)
        target = range(1, args.size_test + 1)
        ilp_result = ilp_insert(input_list, hole_fills, target)
        print("Input is ", input_list)
        print("ILP Result is ", ilp_result)

    if args.test:
        input_list, hole_fills = input_generator(6)
        input_list = ['hole', 4, 'hole', 2, 1, 'hole']
        hole_fills = [3,5,6]
        opts = all_options(input_list, hole_fills)

        # Get the min of all options
        min_index = None
        min_score = 1000000000000
        print("Have ", len(opts), "options")
        print("Have ", len(hole_fills), "holes")
        for i, opt in enumerate(opts):
            dis = edit_distance(opt[:], target)
            if dis < min_score:
                min_score = dis
                min_index = i

        ilp_result = ilp_insert(input_list, hole_fills, target)
        ilp_score = edit_distance(ilp_result[:], target)

        swap_alg_result = swap_alg(input_list, hole_fills, target)
        swap_alg_score = edit_distance(swap_alg_result[:], target)

        # use the in_order_insert algorithm
        # fills = in_order_insert(input_list, hole_fills, target)
        # alg_opt = fill_holes(input_list[:], fills)
        # alg_dist = edit_distance(alg_opt[:], target)

        print("For input ", input_list)
        print("Target ", target)

        print("Min score is ", min_score)
        print("Min config was ", opts[min_index])

        print("ILP Score is ", ilp_score)
        print("ILP result is ", ilp_result)

        # print("Alg score is ", alg_dist)
        # print("Alg config was ", alg_opt)

        print("Swap alg score is ", swap_alg_score)
        print("Swap alg result is ", swap_alg_result)
