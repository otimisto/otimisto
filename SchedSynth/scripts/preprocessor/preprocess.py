import argparse
import math

def main():
    parser = argparse.ArgumentParser(description='Preprocessor for a custom programming language.')
    parser.add_argument('input_file', type=str, help='Path to the source code file to be preprocessed')
    parser.add_argument('output_file', type=str, help='Path to the output file to save the preprocessed code')

    args = parser.parse_args()

    # Read the input file
    with open(args.input_file, 'r') as file:
        source_code = file.read()

    # Process the source code
    processed_code = preprocess(source_code)

    # Write the processed code to the output file
    with open(args.output_file, 'w') as file:
        file.write('\n'.join(processed_code))

def count_leading_spaces(input_string):
    return len(input_string) - len(input_string.lstrip(' '))

def get_nesting_depth(s):
    spaces = count_leading_spaces(s)
    if spaces % 2 != 0:
        print("Error: invalid nesting depth on line ", s)
        return None
    # Round down
    return math.floor((spaces + 1) / 2)

def preprocess(source_code):
    result_lines = []
    # Implement your preprocessing logic here
    # Define the REPEAT pattern

    # Keep track of what adjustments to nesting
    # need to be made
    trigger_points = []
    nests_to_add = []
    for line in source_code.split('\n'):
        if line.strip().startswith('REPEAT'):
            # Parse the line: the syntax is:
            # REPEAT N: <Line>
            split_line = line.split(':')
            if len(split_line) < 2:
                print("Invalid Syntax for Preprocessor: Line", line)
                return
            # REPEAT N
            repeat_command = split_line[0]
            # The command
            line_contents = ':'.join(split_line[1:])
            # The split repeat command REPEAT, N
            split_repeat = repeat_command.strip().split(' ')
            # N
            num_times = int(split_repeat[1])

            starting_nest_depth = get_nesting_depth(repeat_command)
            # Check if this is a command that needs increasing nesting
            # depth
            should_increase_depth = line.strip().endswith(':')
            nest_depth = starting_nest_depth

            for time in range(0, num_times):
                this_time_line_contents = line_contents.replace('{}', str(time))
                result_lines.append('  ' * nest_depth + this_time_line_contents)

                if should_increase_depth:
                    nest_depth += 1

            # We now need to add this nest depth to every child
            # node, until we get back under the starting depth.
            nests_to_add.append(nest_depth - starting_nest_depth)
            trigger_points.append(starting_nest_depth)
        elif line.strip().startswith("IF"):
            # Macro format is: IF TRUE: <Line>
            #              or  IF FALSE: <Line>
            # We assume indentiatiaon as if it is going to be there
            # and then if its not, we remove any nested
            # indenttation.
            cond = line.split(':')[0]
            line_contents = line.split(":")[1:]
            nesting_depth = get_nesting_depth(line)
            if "TRUE" in cond:
                result_lines.append('  ' * nest_depth + line_contents)
            elif "FALSE" in cond:
                # We dont't want to print the line --- but will go through
                # and adjust the nesting of any following commands that
                # were to be nested within this.
                nests_to_add.append(-1)
                trigger_points.append(nest_depth)
            else:
                print("Ill-formatted IF macro.  Must be IF TRUE or IF FALSE, line is " + line)
        else:
            nest_depth = get_nesting_depth(line)
            # Determine how many spaces we need to add/subtract
            # due to previous macros.
            i = 0
            spaces_to_add = 0
            while i < len(trigger_points):
                if nest_depth <= trigger_points[i]:
                    # This is no longer relevant
                    del trigger_points[i]
                    del nests_to_add[i]
                else:
                    ## Otherwise take this into account
                    spaces_to_add += nests_to_add[i]
                    i += 1 # dont increment in initial cond

            if spaces_to_add > 0:
                line = ('  ' * spaces_to_add) + line
            elif spaces_to_add < 0:
                line = line[spaces_to_add * -2:]

            result_lines.append(line)
    return result_lines

if __name__ == '__main__':
    main()
