import os

import ASLPrintVisitor

def recursive_gather_files(directory):
  subdirectories, files = [], []
  for f in os.scandir(directory):
    if f.is_dir():
      subdirectories.append(f.path)
    if f.is_file():
      files.append(f.path)

  for subdirectory in list(subdirectories):
    rec_directories, rec_files = recursive_gather_files(subdirectory)
    subdirectories.extend(rec_directories)
    files.extend(rec_files)
  return subdirectories, files

def run_test_directory(test_directory, all, start, debug = False):
  subfolders, files = recursive_gather_files(test_directory)
  if debug:
    print(f"Found {len(files)} files in {test_directory}")

  test_files = filter (lambda x: x.endswith('.asl'), files)
  test_files = sorted(test_files)
  if debug:
    print(f"Found {len(test_files)} test files")

  successes = 0
  failed_tests = []
  for i in range(start, len(test_files)):
    test_file = test_files[i]
    if debug:
      print(f"{i}/{len(test_files)}: {test_file}")
    with open(test_file, 'r') as f:
      asl_code = f.read()
      errors = ASLPrintVisitor.ASLPrintVisitor().printASL(asl_code)
      if errors:
        print(f'{test_file} has encountered {errors} errors')
        failed_tests.append(test_file)
        if not all:
          break
      else:
        successes += 1
  return f'{successes}/{len(test_files)}', failed_tests

if __name__ == "__main__":
  successes, failed_tests = run_test_directory("../asl_files/", True, 0, True)
  print(f"../asl_files/: {successes}")
  for failed_test in failed_tests:
    print(f"FAILED: {failed_test}")
