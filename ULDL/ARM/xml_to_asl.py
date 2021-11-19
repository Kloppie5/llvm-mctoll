
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

def read_index(index_file, debug = False):
  """
    Reads the index file and returns a list of instructions files.
  """
  xml = ET.parse(index_file)
  files = []
  for iform in xml.findall('.//iforms/iform'):
    if debug:
      print(f"""Found iformfile {iform.get('iformfile')}""")
    files.append(iform.get('iformfile'))
  return files
def extract_asl_instruction(xml_file, debug = False):
  """
    Extracts the ASL from the XML file and saves it to a file in the 'asl_files' directory.
    Skips files that alias other instructions.
    Assumes that the file is well-formed.
  """
  xml = ET.parse(xml_file)
  instructionsection = xml.getroot()

  if instructionsection.tag != 'instructionsection':
    print(f'Encountered malformed documentation file {xml_file}')
    exit(1)
  if instructionsection.find('aliasto') is not None:
    if debug:
      print(f'Skipping {xml_file}')
    return
  if debug:
    print(f"Processing {xml_file}")

  identifier = instructionsection.get('id')

  if not os.path.exists('./asl_files'):
    os.mkdir('asl_files')
  if not os.path.exists('./asl_files/decode'):
    os.mkdir('asl_files/decode')
  if not os.path.exists('./asl_files/execute'):
    os.mkdir('asl_files/execute')

  if os.path.exists(f'./asl_files/execute/{identifier}.asl'):
    if debug:
      print(f'Skipping existing identifier {identifier}')
    return

  # ps_section and ps can technically hold multiple elements, but this does
  # not happen in the current dataset.
  pseudocode_execute = ''.join(instructionsection.find('ps_section/ps/pstext').itertext())
  if debug:
    print(f'Writing execute pseudocode to ./asl_files/execute/{identifier}.asl')
  with open(f'./asl_files/execute/{identifier}.asl', 'w') as f:
    f.write(pseudocode_execute)

  for iclass in instructionsection.find('classes').findall('iclass'):
    encodingclass = iclass.get('name')
    pseudocode_decode = ''.join(iclass.find('ps_section/ps/pstext').itertext())
    if debug:
      print(f'Writing decode pseudocode to ./asl_files/execute/{identifier}.asl')
    with open(f'./asl_files/decode/{identifier}_{encodingclass}.asl', 'w') as f:
      f.write(pseudocode_decode)

def extract_shared_pseudocode(shared_pseudocode_file, debug = False):
  """
    Reads the shared pseudocode file and saves the functions as
    individual asl files in the 'asl_files/shared_pseudocode' directory.
  """
  xml = ET.parse(shared_pseudocode_file)
  instructionsection = xml.getroot()
  ps_section = instructionsection.find('ps_section')

  if not os.path.exists('./asl_files'):
    os.mkdir('asl_files')
  if not os.path.exists('./asl_files/shared_pseudocode'):
    os.mkdir('asl_files/shared_pseudocode')

  for ps in ps_section.findall('ps'):
    identifier = ps.get('name')
    if os.path.exists(f'./asl_files/shared_pseudocode/{identifier}.asl'):
      if debug:
        print(f'Skipping existing identifier {identifier}')
      continue
    if not os.path.exists(os.path.dirname(f'./asl_files/shared_pseudocode/{identifier}.asl')):
      os.makedirs(os.path.dirname(f'./asl_files/shared_pseudocode/{identifier}.asl'))
    pseudocode = ''.join(ps.find('pstext').itertext())
    if debug:
      print(f'Writing shared pseudocode to ./asl_files/shared_pseudocode/{identifier}.asl')
    with open(f'./asl_files/shared_pseudocode/{identifier}.asl', 'w') as f:
      f.write(pseudocode)

if __name__ == "__main__":
  files = read_index('./xml_files/index.xml')
  for f in files:
    extract_asl_instruction(f'./xml_files/{f}')
  extract_shared_pseudocode('./xml_files/shared_pseudocode.xml')
