package com.veadan.folib.validator;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

/**
 * @author veadan
 * @since 2024-09-03 13:51
 */

@Slf4j

public class DpkgMandatoryFieldsValidator  implements DpkgPackageMetadataValidator{

    public void validate(String key, String... values) throws MetadataValidationException {
        log.info("Starting to validate metadata");
        if (StringUtils.isEmpty(key) || values.length == 0 || values[0] == null) {
            String msg = "The Control file is missing a mandatory control field: Package";
            log.error(msg);
            throw new MetadataValidationException(msg);
        }
    }
}
