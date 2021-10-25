from lxml import etree as ET
import os

semantics_files = []
for root, dirs, files in os.walk("./operational_semantics/"):
  for file in files:
    if file.endswith(".xml"):
      semantics_files.append(os.path.join(root, file))
print("Found %d semantics files" % len(semantics_files))

ISAs = {}
ISAs["A32"] = []
ISAs["T32"] = []
for semantics_file in semantics_files:
  print("Processing %s" % semantics_file)
  xml = ET.parse(semantics_file)
  for iclass in xml.findall('.//classes/iclass'):
    isa = iclass.attrib['isa']
    mnemonics = set()
    match_pattern = []
    print_pattern = []
    constraints = []
    for box in iclass.find('regdiagram').findall('box'):
      hibit = box.get('hibit')
      width = box.get('width')
      if width is None:
        width = 1
      name = box.get('name')
      if name is None:
        name = hibit
      settings = box.get('settings')
      constraint = box.get('constraint')

      pattern = ""
      if settings is None or constraint is not None:
        match_pattern.append('-' * int(width))
      pattern += "%s/%s" % (name, width)
      if settings is not None and constraint is None:
        contents = ''.join(c.text for c in box.findall('c'))
        match_pattern.append(contents.replace('(', '').replace(')', ''))
        pattern += ":%s" % contents
      if constraint is not None:
        constraints.append("%s %s" % (name, constraint))
      print_pattern.append(pattern)

    for encoding in iclass.findall('encoding'):
      mnemonic = encoding.xpath('./docvars/docvar[@key="mnemonic"]/@value')[0]
      if mnemonic is not None:
        mnemonics.add(mnemonic)
    ISAs[isa].append({
      "mnemonics" : mnemonics,
      "match_pattern" : match_pattern,
      "print_pattern" : print_pattern,
      "constraints" : constraints
    })

for isa in ISAs:
  print("%s:" % isa)
  for instruction in ISAs[isa]:
    print("%s" % '/'.join(instruction["mnemonics"]))
    print("  %s" % ''.join(instruction["match_pattern"]))
    print("  %s" % ' '.join(instruction["print_pattern"]))
    print("  %s" % ', '.join(instruction["constraints"]))
