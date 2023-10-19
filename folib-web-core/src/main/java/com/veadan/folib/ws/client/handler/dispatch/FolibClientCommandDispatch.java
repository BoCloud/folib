package com.veadan.folib.ws.client.handler.dispatch;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.ws.FolibWsAction;
import com.veadan.folib.ws.client.handler.command.FolibClientCommand;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

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
@Component
public class FolibClientCommandDispatch 
{
    @Autowired
    private ApplicationContext applicationContext;

    private final Map<String, FolibClientCommandRegister> wsCommandRegister = new HashMap<>();

    public void init()
    {
        Optional.of(applicationContext.getBeansOfType(FolibClientCommand.class).values())
                .orElse(Collections.emptyList())
                .forEach(command -> {
                    wsCommandRegister.put(command.command(), new FolibClientCommandRegister()
                            .setCommand(command)
                            .setPayloadClass(this.getPayloadClass(command))
                    );
                    log.info("【FolibClientWs分发器】Dispatcher初始化，命令（{}）注册成功", command.command());
                });
    }
    
    public void dispatch(FolibWsAction action)
    {
        final FolibClientCommandRegister folibClientCommandRegister = wsCommandRegister.get(action.getCommand());
        if (null == folibClientCommandRegister)
        {
            log.error("【FolibClientWs分发器】未知Command（{}）", action.getCommand());
            return;
        }

        final String payload = action.getPayload();
        final Class<?> payloadClass = folibClientCommandRegister.getPayloadClass();
        final FolibClientCommand clientCommandRegisterCommand = folibClientCommandRegister.getCommand();

        if (String.class.equals(payloadClass))
        { // 基础参数类型
            clientCommandRegisterCommand.execute(payload);
        }
        else
        { // 转化对象类型参数
            clientCommandRegisterCommand.execute(JSON.parseObject(payload, payloadClass));
        }
    }

    private Class<?> getPayloadClass(FolibClientCommand<?> agentCommand) {
        // 获取实现接口的类型参数信息
        Type[] genericInterfaces = agentCommand.getClass().getGenericInterfaces();
        final String typeName = ((ParameterizedType) genericInterfaces[0]).getActualTypeArguments()[0].getTypeName();
        try {
            return Class.forName(typeName);
        } catch (ClassNotFoundException e) {
            log.error("【FolibClientWs分发器】Dispatcher初始化，未找到的FolibClientCommand（{}）载体类型", agentCommand.command());
            return null;
        }
    }


    @Data
    @Accessors(chain = true)
    public static class FolibClientCommandRegister 
    {
        public FolibClientCommand<? > command;
        public Class<?> payloadClass;
    }
    
}
