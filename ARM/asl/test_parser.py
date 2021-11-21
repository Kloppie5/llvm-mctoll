import os

import ASLPrintVisitor

def run_test_directory(test_directory, all, start, debug = False):
  successes = 0
  failed_tests = []
  test_files = os.listdir(test_directory)
  test_files = sorted(test_files)
  for i in range(start, len(test_files)):
    test_file = test_files[i]
    if debug:
      print(f"{i}/{len(test_files)}: {test_file}")
    with open(f'{test_directory}{test_file}', 'r') as f:
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
  for test_directory in [
    "../asl_files/decode/",
    "../asl_files/execute/",
  ]:
    successes, failed_tests = run_test_directory(test_directory, False, 0, True)
    print(f"{test_directory}: {successes}")
    for failed_test in failed_tests:
      print(f"FAILED: {failed_test}")
