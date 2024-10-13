package com.veadan.folib.validator;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

/**
 * @author huayanjun
 * @since 2024-09-03 13:49
 */
@Slf4j
public class DpkgProvidesValidator implements DpkgPackageMetadataValidator {
    public void validate(String key, String... values) throws MetadataValidationException {
        log.debug("Starting to validate metadata");
        if (StringUtils.isBlank(key)) {
            log.debug("Provides line is empty. quitting the validator");
        } else if (values != null && values.length != 0) {
            for (String value : values) {
                if (value.isEmpty()) {
                    String msg = "The Control file contains a malformed Provides line; one of the packages is empty";
                    log.error(msg);
                    throw new MetadataValidationException(msg);
                }
            }
        } else {
            String msg = "The Control file contains an empty field: Provides";
            log.error(msg);
            throw new MetadataValidationException(msg);
        }
    }
}
