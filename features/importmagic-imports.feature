Feature: Importmagic fixes symbols in buffers

  Scenario: Fix some symbols in a buffer with unimported symbols
    Given the buffer has correctly started importmagic-mode
    When I insert:
    """
    config_path = os.path.join(os.getcwd(), 'index.json')
    today = datetime.now()
    future = datetime.timedelta(hours=1)
    """
    And I call "importmagic-fix-imports" and accept "4" times
    Then I should see "import os.path"
    And I should see "import os"
    And I should see "import datetime"

  Scenario: Figure out unresolved symbols in a bad buffer
    Given the buffer has correctly started importmagic-mode
    When I insert:
    """
    config_path = os.path.join(os.getcwd(), 'index.json')
    today = datetime.now()
    future = datetime.timedelta(hours=1)
    """
    Then an unresolved symbol should be "os.path.join"
    And an unresolved symbol should be "os.getcwd"
    And an unresolved symbol should be "datetime.timedelta"
    And an unresolved symbol should be "datetime.now"

  Scenario: Attempt to fix import for os.path symbol
    Given the buffer has correctly started importmagic-mode
    When I insert "os.path"
    And I go to beginning of buffer
    And I call "importmagic-fix-symbol-at-point" and accept "1" time
    Then I should see "import os"
    And I should see "os.path"

  Scenario: Attempt to manually fix symbol os
    Given the buffer has correctly started importmagic-mode
    When I insert "os.path"
    And I go to beginning of buffer
    And I query for symbol "os" accepting the first candidate
    Then I should see "import os"
    And I should see "os.path"
