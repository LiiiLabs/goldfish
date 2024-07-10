find . | grep "test.scm" | grep -v "srfi-78-test" | xargs -I% s7 %
