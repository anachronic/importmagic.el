Feature: Importmagic can switch python interpreter correctly

  Scenario: Change interpreter to ipython
    Given I set importmagic-python-interpreter to "ipython"
    And the buffer has correctly started importmagic-mode
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
