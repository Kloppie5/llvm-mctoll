
"""
a32_encindex.xml
fpsimdindex.xml
index.xml
shared_pseudocode.xml
t32_encindex.xml
"""

import os
import sys

from lxml import etree as ET

def read_boxes(boxes, debug = True):
  fields = {}
  contents = {}
  for box in boxes.findall('box'):
    hibit = box.get('hibit')
    width = box.get('width')
    if width is None:
      width = 1
    name = box.get('name')
    if name is None:
      name = f'{hibit}/{width}'
    content = ''.join(''.join(c.itertext()) for c in box.findall('c'))
    fields[name] = {'hibit': hibit, 'width': width}
    if content != '':
      contents[name] = content
  return fields, contents

def fields_to_pattern(fields, contents, length):
  newcontents = {}
  for key in contents:
    if key.find(':') != -1:
      keys = key.split(':')
      index = 0
      for k in keys:
        if contents[key].startswith('!= '):
          newcontents[k] = '!= ' + contents[key][index+3:int(fields[k]['width'])+index+3]
        else:
          newcontents[k] = contents[key][index:int(fields[k]['width'])+index]
        index += int(fields[k]['width'])
    else:
      newcontents[key] = contents[key]
  contents = newcontents

  include_pattern = []
  exclude_pattern = []
  index = length - 1
  for key in fields:
    field = fields[key]
    content = ''
    if key in contents:
      content = contents[key]


    diff = index - int(field['hibit'])
    if diff > 0:
      include_pattern.append('-'*diff)
      exclude_pattern.append('-'*diff)
    blank = '-' * int(field['width'])
    if content == '':
      include_pattern.append(blank)
      exclude_pattern.append(blank)
    elif content.startswith('!= '):
      include_pattern.append(blank)
      exclude_pattern.append(content[3:])
    else:
      include_pattern.append(content)
      exclude_pattern.append(blank)
    index = int(field['hibit']) - int(field['width'])
  if index > -1:
    include_pattern.append('-'*(index+1))
    exclude_pattern.append('-'*(index+1))

  return ''.join(include_pattern), ''.join(exclude_pattern)

def node_sub_match_tree(root, node, pattern_length, debug = True):
  groupname = node.get('groupname')
  iclass = node.get('iclass')
  fields, contents = read_boxes(node.find('decode'))
  include_pattern, exclude_pattern = fields_to_pattern(fields, contents, pattern_length)

  name = iclass
  children = []
  if groupname is not None:
    name = groupname
    for child_node in node.findall('node'):
      children.append(node_sub_match_tree(root, child_node, pattern_length, debug))
  if iclass is not None:
    iclass_sect = root.find(f'iclass_sect[@id="{iclass}"]')
    if iclass_sect is not None:
      children = iclass_sect_sub_match_tree(root, iclass_sect, pattern_length, debug)

  if debug:
    print(f'{name:35}: {include_pattern} | {exclude_pattern}')

  return {
    'name': name,
    'include': include_pattern,
    'exclude': exclude_pattern,
    'children': children,
  }

def iclass_sect_sub_match_tree(root, iclass_sect, pattern_length, debug = True):
  fields, contents = read_boxes(iclass_sect.find('regdiagram'))
  instructiontable = iclass_sect.find('instructiontable')
  children = []
  thead = instructiontable.find('thead')
  bitfields = thead.findall('tr/th[@class="bitfields"]')

  tbody = instructiontable.find('tbody')
  for tr in tbody.findall('tr'):
    encname = tr.get('encname')
    if encname is None:
      continue
    tds = tr.findall('td[@class="bitfield"]')
    iformname = tr.find('td[@class="iformname"]')
    iformid = iformname.get('iformid')
    subcontents = {}
    for i in range(len(bitfields)):
      bitfield = bitfields[i].text
      subcontent = tds[i].text
      if subcontent is not None:
        subcontents[bitfield] = subcontent
    include_pattern, exclude_pattern = fields_to_pattern(fields, subcontents, pattern_length)

    if debug:
      print(f'{encname:35}: {include_pattern} | {exclude_pattern}')

    children.append({
      'name': encname,
      'encname': encname,
      'iformid': iformid,
      'include': include_pattern,
      'exclude': exclude_pattern,
    })
  return children

def build_match_tree(encindex_file, debug = False):
  """
    Reads the encoding index file and returns a match tree.
  """
  xml = ET.parse(encindex_file)
  encodingindex = xml.getroot()
  instructionset = encodingindex.get('instructionset')
  hierarchy = encodingindex.find('hierarchy')

  pattern_length = 32

  fields, contents = read_boxes(hierarchy.find('regdiagram'))
  include_pattern, exclude_pattern = fields_to_pattern(fields, contents, pattern_length)

  children = []
  for child_node in hierarchy.findall('node'):
    sub_tree = node_sub_match_tree(encodingindex, child_node, pattern_length, debug)
    if sub_tree is not None:
      children.append(sub_tree)

  return {
    'name': instructionset,
    'include': include_pattern,
    'exclude': exclude_pattern,
    'children': children,
  }

def print_match_tree(tree, indent = 0):
  print(f"""{tree['name']:35}{' '*indent}:{' '*(10-indent)} {tree['include']} | {tree['exclude']}""")
  if 'children' in tree:
    for child in tree['children']:
      print_match_tree(child, indent + 2)

if __name__ == "__main__":
  MTa32 = build_match_tree('./xml_files/a32_encindex.xml')
  MTt32 = build_match_tree('./xml_files/t32_encindex.xml')
  print_match_tree(MTa32)
  print_match_tree(MTt32)
