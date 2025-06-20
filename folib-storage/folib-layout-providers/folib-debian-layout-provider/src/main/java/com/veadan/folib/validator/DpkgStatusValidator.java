package com.veadan.folib.validator;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;

/**
 * @author huayanjun
 * @since 2024-09-03 13:47
 */
@Slf4j
public class DpkgStatusValidator implements DpkgPackageMetadataValidator{

    public void validate(String key, String... values) throws MetadataValidationException {
        log.trace("Starting to validate metadata");
        if (StringUtils.isBlank(key)) {
            log.debug("Status line is empty. quitting the validator");
        } else {
            String msg;
            if (values == null) {
                msg = "The Control file contains an empty field: Status";
                log.error(msg);
                throw new MetadataValidationException(msg);
            } else if (values.length != 3) {
                msg = "The Control file contains an invalid amount of params for field: Status (3 !=" + values.length + ")";
                log.error(msg);
                if (log.isDebugEnabled()) {
                    log.debug("The Status field values are: {}", Arrays.toString(values));
                }
                throw new MetadataValidationException(msg);
            }
        }
    }
}
