### Build Process
## Initialization Stage
## Enumeration Stage
## Processing Stage
if(NOT CMAKE_BUILD_EARLY_EXPANSION)
  add_definitions(
    -DEI_SENSOR_AQ_STREAM=FILE
    -DEI_CLASSIFIER_TFLITE_ENABLE_ESP_NN=1
    -DEI_PORTING_ARDUINO=0
    ) # enables ESP-NN optimizations by Espressif
endif()
## Finalization Stage

### Main
set(MODEL_FOLDER edge-impulse-sdk/model)
set(INFERENCE_FOLDER edge-impulse-sdk/inference)
set(INGESTION_FOLDER edge-impulse-sdk/ingestion)
set(PLATFORM_FOLDER edge-impulse-sdk/platform)
set(FIRMWARE_FOLDER edge-impulse-sdk/firmware)
set(UTILITY_FOLDER edge-impulse-sdk/utility)

set(include_dirs 
    ${MODEL_FOLDER}
    ${MODEL_FOLDER}/tflite-model
    ${MODEL_FOLDER}/model-parameters
    ${INFERENCE_FOLDER}
    ${INFERENCE_FOLDER}/anomaly
    ${INFERENCE_FOLDER}/classifier
    ${INFERENCE_FOLDER}/dsp
    ${INFERENCE_FOLDER}/porting
    ${INFERENCE_FOLDER}/tensorflow
    ${INFERENCE_FOLDER}/third_party
    ${INFERENCE_FOLDER}/third_party/ruy
    ${INFERENCE_FOLDER}/third_party/gemmlowp
    ${INFERENCE_FOLDER}/third_party/flatbuffers/include
    ${INGESTION_FOLDER}
    ${PLATFORM_FOLDER}/espressif_esp32
    ${PLATFORM_FOLDER}/sensors
    ${FIRMWARE_FOLDER}
    ${FIRMWARE_FOLDER}/at-server
    ${UTILITY_FOLDER}/mbedtls_hmac_sha256_sw
    ${UTILITY_FOLDER}/QCBOR/inc
    ${UTILITY_FOLDER}/QCBOR/src
    ${UTILITY_FOLDER}/include-path-fix
)

include(${COMPONENT_DIR}/${INFERENCE_FOLDER}/cmake/utils.cmake)

## inference files
RECURSIVE_FIND_FILE_EXCLUDE_DIR(SOURCE_FILES "${INFERENCE_FOLDER}" "CMSIS" "*.cpp")
RECURSIVE_FIND_FILE_EXCLUDE_DIR(CC_FILES "${INFERENCE_FOLDER}" "CMSIS" "*.cc")
RECURSIVE_FIND_FILE_EXCLUDE_DIR(S_FILES "${INFERENCE_FOLDER}" "CMSIS" "*.s")
RECURSIVE_FIND_FILE_EXCLUDE_DIR(C_FILES "${INFERENCE_FOLDER}" "CMSIS" "*.c")
RECURSIVE_FIND_FILE(PORTING_FILES "${INFERENCE_FOLDER}/porting/espressif" "*.cpp")
list(APPEND SOURCE_FILES ${S_FILES})
list(APPEND SOURCE_FILES ${C_FILES})
list(APPEND SOURCE_FILES ${CC_FILES})
list(APPEND SOURCE_FILES ${PORTING_FILES})

## firmware files
RECURSIVE_FIND_FILE(FIRMWARE_SDK_FILES "${FIRMWARE_FOLDER}" "*.cpp")
list(APPEND SOURCE_FILES ${FIRMWARE_SDK_FILES})

## model files
RECURSIVE_FIND_FILE(MODEL_FILES "${MODEL_FOLDER}/tflite-model" "*.cpp")
list(APPEND SOURCE_FILES ${MODEL_FILES})

## platform files
RECURSIVE_FIND_FILE(PLATFORM_FILES "${PLATFORM_FOLDER}/espressif_esp32" "*.cpp")
RECURSIVE_FIND_FILE(SENSORS_FILES "${PLATFORM_FOLDER}/sensors" "*.cpp")
list(APPEND SOURCE_FILES ${PLATFORM_FILES})
list(APPEND SOURCE_FILES ${SENSORS_FILES})

## ingestion files
RECURSIVE_FIND_FILE(INGESTION_FILES "${INGESTION_FOLDER}" "*.cpp")
list(APPEND SOURCE_FILES ${INGESTION_FILES})

## utility files
RECURSIVE_FIND_FILE(QCBOR_FILES "${UTILITY_FOLDER}/QCBOR/src" "*.c")
RECURSIVE_FIND_FILE(MBEDTLS_FILES "${UTILITY_FOLDER}/mbedtls_hmac_sha256_sw/mbedtls/src" "*.c")
list(APPEND SOURCE_FILES ${QCBOR_FILES})
list(APPEND SOURCE_FILES ${MBEDTLS_FILES})

idf_component_register(
  SRCS "main.cc" "tinyml.cc" "${SOURCE_FILES}"
  INCLUDE_DIRS "." "${include_dirs}")

target_compile_options(${COMPONENT_LIB} PRIVATE "-Wno-error=format")
target_compile_options(${COMPONENT_LIB} PRIVATE "-Wno-error=nonnull-compare")
target_compile_options(${COMPONENT_LIB} PRIVATE "-Wno-error=stringop-truncation")
target_compile_options(${COMPONENT_LIB} PRIVATE -O3)
