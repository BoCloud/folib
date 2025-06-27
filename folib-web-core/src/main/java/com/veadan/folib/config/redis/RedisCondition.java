//package com.veadan.folib.config.redis;
//
//import org.springframework.context.annotation.Condition;
//import org.springframework.context.annotation.ConditionContext;
//import org.springframework.core.type.AnnotatedTypeMetadata;
//
///**
// * @author veadan
// **/
//public class RedisCondition implements Condition {
//
//    /**
//     * ConditionContext context: spring容器上下文环境
//     * AnnotatedTypeMetadata metadata ：@Conditional修饰类型信息
//     */
//    @Override
//    public boolean matches(ConditionContext context, AnnotatedTypeMetadata metadata) {
//        String systemName = context.getEnvironment().getProperty("spring.redis.enabled");
//        if (Boolean.TRUE.toString().equals(systemName)) {
//            return true;
//        }
//        return false;
//    }
//}
