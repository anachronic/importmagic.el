Feature: Importmagic can start

  Scenario: importmagic-mode can start
    When I turn on python-mode
    And I turn on importmagic-mode
    Then importmagic-server should be up

  Scenario: importmagic uses python as interpreter and starts correctly
    Given I set importmagic-python-interpreter to "python"
    When I turn on python-mode
    And I turn on importmagic-mode
    Then importmagic-server should be up

  Scenario: importmagic does not start on fundamental-mode
    Given buffer is in fundamental-mode
    When I try to turn on importmagic-mode
    Then importmagic-server should not be up

  Scenario: importmagic doesn't start in c-mode
    Given buffer is in c-mode
    When I try to turn on importmagic-mode
    Then importmagic-server should not be up


  Scenario: importmagic can't start without importmagic-mode off but python-mode on
    When I turn on python-mode
    Then importmagic-server should not be up
