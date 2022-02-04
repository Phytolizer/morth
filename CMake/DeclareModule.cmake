function(declare_module NAME)
    cmake_parse_arguments(
        PARSE_ARGV 0 "DM" "INTERNAL_INCLUDE" "KIND;TARGET_NAME;OUTPUT_NAME"
        "SOURCES;ABSOLUTE_SOURCES;DEPENDS;INCLUDES"
    )
    if(NOT DM_KIND)
        message(FATAL_ERROR "Missing module kind")
    endif()
    if(DM_KIND STREQUAL "library")
        set(DM_PUBLICITY PUBLIC)
    elseif(DM_KIND STREQUAL "executable")
        set(DM_PUBLICITY PRIVATE)
    elseif(DM_KIND STREQUAL "interface")
        set(DM_KIND "library")
        set(DM_INTERFACEKW INTERFACE)
        set(DM_PUBLICITY INTERFACE)
    else()
        message(FATAL_ERROR "Unknown module kind")
    endif()
    if(DM_TARGET_NAME)
        set(DM_TARGET_NAME ${DM_TARGET_NAME})
    else()
        set(DM_TARGET_NAME ${NAME})
    endif()
    list(TRANSFORM DM_SOURCES PREPEND "Modules/${NAME}/Source/")
    cmake_language(
        CALL "add_${DM_KIND}" ${DM_TARGET_NAME} ${DM_INTERFACEKW} ${DM_SOURCES}
        ${DM_ABSOLUTE_SOURCES}
    )
    target_link_libraries(${DM_TARGET_NAME} ${DM_PUBLICITY} ${DM_DEPENDS})
    target_include_directories(
        ${DM_TARGET_NAME} ${DM_PUBLICITY} ${DM_INCLUDES}
        "Modules/${NAME}/Include"
    )
    target_compile_options(
        ${DM_TARGET_NAME}
        ${DM_PUBLICITY}
        -Wall
        -Wextra
        -Werror=return-type
        -Werror=unused-function
        -Werror=unused-parameter
        -Werror=unused-variable
        -Werror=switch
        -fsanitize=address,undefined
    )
    target_link_options(
        ${DM_TARGET_NAME} ${DM_PUBLICITY} -fsanitize=address,undefined
    )
    if(DM_INTERNAL_INCLUDE)
        target_include_directories(
            ${DM_TARGET_NAME} PRIVATE "Modules/${NAME}/InternalInclude"
        )
    endif()
    if(DM_OUTPUT_NAME)
        set_target_properties(
            ${DM_TARGET_NAME} PROPERTIES OUTPUT_NAME ${DM_OUTPUT_NAME}
        )
    endif()
    if(WIN32)
        target_compile_definitions(
            ${DM_TARGET_NAME} ${DM_PUBLICITY} _CRT_SECURE_NO_WARNINGS
        )
    endif()
endfunction()
