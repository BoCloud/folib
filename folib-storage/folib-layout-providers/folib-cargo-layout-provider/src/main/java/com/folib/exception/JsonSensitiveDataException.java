package com.folib.exception;


import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.yaml.snakeyaml.scanner.ScannerException;

public class JsonSensitiveDataException extends JsonParsingException {
    private static final String SENSITIVE_DATA_REGEX = "([Pp]assword|[Kk]ey)(:.+$)";

    private static final Pattern PATTERN = Pattern.compile("([Pp]assword|[Kk]ey)(:.+$)", 8);

    private static final String MASK = ": ******";

    public JsonSensitiveDataException(Exception e) {
        super(maskSensitiveInputs(e));
        if (isCausedByScannerExceptionWithSensitiveData(e)) {
            Throwable rootCause = Optional.<Throwable>ofNullable(ExceptionUtils.getRootCause(e)).orElse(e);
            setStackTrace(rootCause.getStackTrace());
        } else {
            initCause(e);
        }
    }

    private static String maskSensitiveInputs(Exception e) {
        String result = "";
        if (e != null) {
            String message = e.toString();
            if (StringUtils.isNotBlank(message)) {
                result = message;
                if (isCausedByScannerExceptionWithSensitiveData(e)) {
                    Matcher matcher = PATTERN.matcher(message);
                    while (matcher.find())
                        result = result.replace(matcher.group(2), ": ******");
                }
            }
        }
        return result;
    }

    private static boolean isCausedByScannerExceptionWithSensitiveData(Exception e) {
        boolean result = false;
        if (e != null && ExceptionUtils.indexOfType(e, ScannerException.class) != -1)
            result = recursivelyCheckForSensitiveData(e);
        return result;
    }

    private static boolean recursivelyCheckForSensitiveData(Throwable e) {
        boolean result = false;
        String message = e.toString();
        if (StringUtils.isNotBlank(message)) {
            Matcher matcher = PATTERN.matcher(message);
            if (matcher.find()) {
                result = true;
            } else if (e.getCause() != null) {
                result = recursivelyCheckForSensitiveData(e.getCause());
            }
        }
        return result;
    }
}

