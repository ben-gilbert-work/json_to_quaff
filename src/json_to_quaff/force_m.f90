module json_to_quaff_force_m
  use quaff, only: &
    fallible_force_t, &
    fallible_force_unit_t, &
    parse_force, &
    parse_force_unit
  use rojff, only: &
    fallible_json_value_t, &
    json_value_t, &
    json_string_t
  use erloff, only: &
    error_list_t, &
    module_t, &
    procedure_t, &
    fatal_t

implicit none

interface fallible_force_t
    module procedure fallible_force_from_fallible_json_value
end interface
interface fallible_force_unit_t
  module procedure fallible_force_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_force_m"

contains

function fallible_force_from_fallible_json_value( &
  fallible_json_value) result(fallible_force)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_force_t) :: fallible_force

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_force_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_force = fallible_force_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%value_)
          type is (json_string_t)
              fallible_force = fallible_force_t( &
                  parse_force(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_force = fallible_force_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_force_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_force_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_force_unit_t) :: fallible_force_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_force_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_force_unit = fallible_force_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%value_)
      type is (json_string_t)
        fallible_force_unit = fallible_force_unit_t( &
            parse_force_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_force_unit = fallible_force_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

end module
