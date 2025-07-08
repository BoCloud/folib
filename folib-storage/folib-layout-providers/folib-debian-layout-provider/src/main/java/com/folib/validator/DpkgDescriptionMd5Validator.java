package com.folib.validator;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

/**
 * @author veadan
 * @since 2024-09-03 13:54
 */
@Slf4j
public class DpkgDescriptionMd5Validator implements DpkgPackageMetadataValidator{
    public void validate(String key, String... value) throws MetadataValidationException {
        log.debug("Starting to validate metadata");
        if (StringUtils.isBlank(key)) {
            log.trace("Description-Md5 line is empty. quitting the validator");
        } else if (value.length != 32) {
            String msg = "Control file contains a malformed Description-md5 line; doesn't have the required length (32 != " + value.length + ")";
            log.error(msg);
            throw new MetadataValidationException(msg);
        }
    }
}
