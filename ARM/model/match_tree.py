
def add_instruction(tree, instruction, debug = False):
  main_pattern = []
  sub_pattern = []
  for part in instruction['pattern']:
    blank = '-' * int(part['width'])
    main_set = False
    sub_set = False
    if 'contents' in part:
      contents = part['contents'].replace('(', '').replace(')', '')
      main_set = ('/' in part['name'])
      sub_set = not main_set

    main_pattern.append(contents if main_set else blank)
    sub_pattern.append(contents if sub_set else blank)

  main_pattern = ''.join(main_pattern)
  sub_pattern = ''.join(sub_pattern)

  if main_pattern not in tree:
    tree[main_pattern] = {}
  if sub_pattern not in tree[main_pattern]:
    tree[main_pattern][sub_pattern] = {}

  if 'instructions' not in tree[main_pattern][sub_pattern]:
    tree[main_pattern][sub_pattern]['instructions'] = []
  tree[main_pattern][sub_pattern]['instructions'].append(instruction)

  if debug:
    print(f"Added {instruction['mnemonics']} to tree")
    print(f"  main pattern: {main_pattern}")
    print(f"  sub pattern:  {sub_pattern}")
    print()

def to_byte_tree(tree, debug = False):
  byte_tree = {}
  for main_pattern in tree:
    if debug:
      print(f"Processing main pattern: {main_pattern}")
    sub_tree = byte_tree

    for i in range(0, len(main_pattern), 8):
      byte = main_pattern[i:i+8]
      if byte == '--------':
        continue
      byte_pattern = '-' * i + byte + '-' * (len(main_pattern) - i - 8)
      if byte_pattern not in sub_tree:
        sub_tree[byte_pattern] = {}
      sub_tree = sub_tree[byte_pattern]

    for sub_pattern in tree[main_pattern]:
      if debug:
        print(f"Processing sub pattern: {sub_pattern}")
      subsub_tree = sub_tree

      for i in range(0, len(sub_pattern), 8):
        byte = sub_pattern[i:i+8]
        if byte == '--------':
          continue
        byte_pattern = '-' * i + byte + '-' * (len(sub_pattern) - i - 8)
        if byte_pattern not in subsub_tree:
          subsub_tree[byte_pattern] = {}
        subsub_tree = subsub_tree[byte_pattern]

      if 'instructions' not in subsub_tree:
        subsub_tree['instructions'] = []
      for instruction in tree[main_pattern][sub_pattern]['instructions']:
        subsub_tree['instructions'].append(instruction)

  return byte_tree

def build_tree(instructions, debug = False):
  tree = {}
  for instruction in instructions:
    add_instruction(tree, instruction, debug)

  if debug:
    print(f"Added {len(instructions)} instructions to tree")
    print(f"Tree has {len(tree)} main patterns")

  return tree

def print_tree(tree):
  for pattern in tree:
    if 'instructions' in tree[pattern]:
      print(f"{pattern} | {', '.join('/'.join(instruction['mnemonics']) for instruction in tree[pattern]['instructions'])}")
    else:
      print(f"{pattern}")
      print_tree(tree[pattern])
