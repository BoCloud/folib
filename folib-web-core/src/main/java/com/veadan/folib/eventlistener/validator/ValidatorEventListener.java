package com.veadan.folib.eventlistener.validator;

import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.validator.ValidatorEvent;
import com.veadan.folib.event.validator.ValidatorEventTypeEnum;
import com.veadan.folib.task.AlarmNoticeTask;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

@Slf4j
@Component
public class ValidatorEventListener {

    @Inject
    @Lazy
    private AlarmNoticeTask alarmNoticeTask;

    @AsyncEventListener
    public void handle(ValidatorEvent event){

        if(ValidatorEventTypeEnum.STORAGE_VALIDATOR.getType() == event.getType()){
            log.info("storage validator event");
            alarmNoticeTask.immediateExecutionNotice();
        }
    }
}
