Feature: Importmagic starts and finishes correctly

  Scenario: Activate importmagic-mode in a python buffer
    When I turn on python-mode
    And I try to turn on importmagic-mode
    Then importmagic-server should be up

  Scenario: Try to activate importmagic in fundamental-mode
    Given buffer is in fundamental-mode
    When I try to turn on importmagic-mode
    Then importmagic-server should not be up

  Scenario: Try to activate importmagic in c-mode
    Given buffer is in c-mode
    When I try to turn on importmagic-mode
    Then importmagic-server should not be up

  Scenario: Try to deactivate importmagic on a non-python buffer when it's not active
    Given buffer is in c-mode
    When I turn off minor mode importmagic-mode
    Then nothing should happen
    And the buffer should be empty
    And I should not see message "Importmagic and/or epc not found. importmagic.el will not be working."

  Scenario: Try to deactivate importmagic on a python buffer when it's not active
    Given buffer is in python-mode
    When I turn off minor mode importmagic-mode
    Then nothing should happen
    And the buffer should be empty
    And I should not see message "Importmagic and/or epc not found. importmagic.el will not be working."

  Scenario: Check importmagic-server with ONLY python mode
    When I turn on python-mode
    Then importmagic-server should not be up

  Scenario: Activate and deactivate importmagic-mode
    Given buffer is in python-mode
    When I turn on importmagic-mode
    And I turn off minor mode importmagic-mode
    Then importmagic-server should not be up

  Scenario: Activate importmagic in two python buffers
    Given I am in buffer "*importmagic-first-buffer*"
    When I turn on python-mode
    And I turn on importmagic-mode
    And I switch to buffer "*importmagic-second-buffer*"
    And I turn on python-mode
    And I turn on importmagic-mode
    Then buffer "*importmagic-first-buffer*" should have importmagic-server up
    And buffer "*importmagic-second-buffer*" should have importmagic-server up
    And importmagic-server should differ between buffers "*importmagic-first-buffer*" and "*importmagic-second-buffer*"
