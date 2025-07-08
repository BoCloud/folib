package com.folib.util;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.components.DistributedCacheComponent;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2025/2/24
 **/
@Slf4j
public class DirectoryValidatorUtils {

    /**
     * 自定义黑名单字符，路径不允许存在\和?
     */
    private static final String INVALID_CHARS = "\\\\?";

    private static final Pattern INVALID_PATTERN = Pattern.compile("[" + Pattern.quote(INVALID_CHARS) + "]");

    public static boolean validateDirectoryPath(String path) {
        try {
            if (StringUtils.isBlank(path)) {
                return true;
            }
            if (path.trim().isEmpty()) {
                log.error("目录名不能全是空格");
                return false;
            }
            // 检查黑名单字符
            if (getInvalidPattern().matcher(path).find()) {
                log.error("目录名包含非法字符: [{}]", path);
                return false;
            }
            // 检查操作系统路径合法性
            try {
                Paths.get(path);
            } catch (InvalidPathException ex) {
                log.error("非法路径 name [{}] error [{}]", path, ex.getMessage());
                return false;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return true;
    }

    public static Pattern getInvalidPattern() {
        String pathInvalidPatternKey = "PATH_INVALID_CHARS";
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        String pathInvalidPatternValue = distributedCacheComponent.get(pathInvalidPatternKey);
        if (StringUtils.isBlank(pathInvalidPatternValue)) {
            return INVALID_PATTERN;
        }
        return Pattern.compile("[" + Pattern.quote(pathInvalidPatternValue) + "]");
    }
}
