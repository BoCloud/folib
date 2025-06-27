package com.veadan.folib.util;

import com.veadan.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

/**
 * @author veadan
 * @date 2023/10/10
 **/
@Slf4j
public class UserUtils {

    /**
     * 获取登录用户名
     *
     * @return 用户名
     */
    public static String getUsername() {
        String username = "";
        try {
            SecurityContext securityContext = SecurityContextHolder.getContext();
            if (Objects.isNull(securityContext)) {
                return username;
            }
            Authentication authentication = securityContext.getAuthentication();
            if (Objects.isNull(authentication)) {
                return username;
            }
            Object o = authentication.getPrincipal();
            String anonymousUser = "anonymousUser";
            if (anonymousUser.equals(o.toString())) {
                return anonymousUser;
            }
            if (!(o instanceof SpringSecurityUser)) {
                return username;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) o;
            username = userDetails.getUsername();
        } catch (Exception ex) {
            log.warn("获取登录用户名错误 [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return username;
    }
}
