package com.veadan.folib.aop;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.licence.ActivateVo;
import com.veadan.folib.services.CodeActivateService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.servlet.HandlerMapping;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;
import java.util.Objects;

/**
 * license切面
 *
 * @author leipenghui
 * @date 2023/6/27
 **/
@Slf4j
@Aspect
@Order(1)
@Component
public class LicenseAspect {

    @Autowired
    private CodeActivateService codeActivateService;

    @Autowired
    private ConfigurationManager configurationManager;

    /**
     * 定义切入点，切入点为带有@LicenseAnnotation注解的中的所有函数
     */
    @Pointcut("@annotation(com.veadan.folib.annotation.LicenseAnnotation)")
    public void brokerAspect() {

    }

    /**
     * 前置通知
     */
    @Before(value = "brokerAspect()")
    public void before() {
        log.debug("[{}] License切面，开始校验License", this.getClass().getSimpleName());
        ServletRequestAttributes servletRequestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        if (Objects.nonNull(servletRequestAttributes)) {
            HttpServletRequest request = servletRequestAttributes.getRequest();
            Object obj = request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
            if (obj instanceof Map) {
                Map<String, String> map = (Map) obj;
                String storageId = map.getOrDefault("storageId", "");
                String repositoryId = map.getOrDefault("repositoryId", "");
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                    Repository repository = configurationManager.getRepository(storageId, repositoryId);
                    if (Objects.nonNull(repository)) {
                        log.debug("[{}] License切面，存储空间 [{}] 仓库 [{}] 已存在，跳过校验", this.getClass().getSimpleName(), storageId, repositoryId);
                        return;
                    }
                }
            }
        }
        ActivateVo activateVo = null;
        try {
            activateVo = codeActivateService.isNotActivate();
        } catch (Exception ex) {
            log.error("[{}] License切面，获取License错误 [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("获取License错误");
        }
        if (Objects.isNull(activateVo)) {
            log.warn("[{}] License切面，License不存在", this.getClass().getSimpleName());
            throw new RuntimeException("请检查License是否存在");
        }
        if (StringUtils.isBlank(activateVo.getMac())) {
            log.warn("[{}] License切面，获取mac错误", this.getClass().getSimpleName());
            throw new RuntimeException("获取机器码错误");
        }
        if (activateVo.isHaveError()) {
            log.warn("[{}] License切面，License不存在 mac [{}]", this.getClass().getSimpleName(), activateVo.getMac());
            throw new RuntimeException("请检查License是否存在");
        }
        if (activateVo.isDalyOut()) {
            log.warn("[{}] License切面，License已过期 mac [{}]", this.getClass().getSimpleName(), activateVo.getMac());
            throw new RuntimeException("请续期后再添加制品存储空间、仓库");
        }
        log.debug("[{}] License切面，License校验通过 mac [{}]", this.getClass().getSimpleName(), activateVo.getMac());
    }

}
