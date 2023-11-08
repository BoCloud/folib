package com.veadan.folib.ws.common;

import com.alibaba.fastjson.JSON;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 19:40
 * @since x.x.x
 */
@Slf4j
public abstract class FolibWsCommandDispatch<T extends FolibWsCommand>
{
    private ApplicationContext applicationContext;

    private static final Map<String, FolibCommandRegister> wsCommandRegister = new HashMap<>();

    public FolibWsCommandDispatch(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    public abstract Class<T> getFolibWsCommandClass();
    
    public void init()
    {
        Optional.of(applicationContext.getBeansOfType(this.getFolibWsCommandClass()).values())
                .orElse(Collections.emptyList())
                .forEach(command -> {
                    wsCommandRegister.put(command.command(), new FolibCommandRegister()
                            .setCommand(command)
                            .setPayloadClass(this.getPayloadClass(command))
                    );
                    log.info("【{}分发器】Dispatcher初始化，命令（{}）注册成功", this.getClass().getSimpleName(), command.command());
                });
    }
    
    public static void dispatch(FolibWsAction action)
    {
        final FolibCommandRegister folibClientCommandRegister = wsCommandRegister.get(action.getCommand());
        if (null == folibClientCommandRegister)
        {
            log.error("【FolibWs分发器】未知Command（{}）",  action.getCommand());
            return;
        }

        final String payload = action.getPayload();
        final Class<?> payloadClass = folibClientCommandRegister.getPayloadClass();
        final FolibWsCommand command = folibClientCommandRegister.getCommand();

        if (String.class.equals(payloadClass))
        { // 基础参数类型
            command.execute(payload);
        }
        else
        { // 转化对象类型参数
            command.execute(JSON.parseObject(payload, payloadClass));
        }
    }

    private Class<?> getPayloadClass(T agentCommand) {
        // 获取实现接口的类型参数信息
        Type[] genericInterfaces = agentCommand.getClass().getGenericInterfaces();
        final String typeName = ((ParameterizedType) genericInterfaces[0]).getActualTypeArguments()[0].getTypeName();
        try {
            return Class.forName(typeName);
        } catch (ClassNotFoundException e) {
            log.error("【{}分发器】Dispatcher初始化，未找到的{}（{}）载体类型", this.getClass().getSimpleName(), 
                    agentCommand.getClass().getSimpleName(), agentCommand.command());
            return null;
        }
    }


    @Data
    @Accessors(chain = true)
    public static class FolibCommandRegister <T extends FolibWsCommand<?>>
    {
        public T command;
        public Class<?> payloadClass;
    }
    
}
