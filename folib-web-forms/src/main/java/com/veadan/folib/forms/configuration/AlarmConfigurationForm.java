package com.veadan.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.veadan.folib.configuration.AlarmConfiguration;
import com.veadan.folib.configuration.MutableAlarmConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Email;
import javax.validation.constraints.Size;
import java.util.List;
import java.util.Optional;

/**
 * 告警配置表单
 */
@Data
@Accessors(chain = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AlarmConfigurationForm {

    //通知策略
    private List<String> notificationPolicy;

    //指定用户列表
    private List<String> recipients;

    //指定邮箱列表
    private List<String>  emails;

    private String cronExpression;

    public AlarmConfigurationForm(List<String> notificationPolicy, List<String> recipients, List<String> emails) {
        this.notificationPolicy = notificationPolicy;
        this.recipients = recipients;
        this.emails = emails;
    }

    @JsonIgnore()
    public AlarmConfiguration toAlarmConfiguration() {
        return new AlarmConfiguration(new MutableAlarmConfiguration(notificationPolicy, recipients, emails));
    }

    @JsonIgnore()
    public MutableAlarmConfiguration getMutableAlarmConfiguration() {
        return new MutableAlarmConfiguration(notificationPolicy, recipients, emails);
    }
    @JsonIgnore()
    public static AlarmConfigurationForm formConfiguration(AlarmConfiguration source) {
        AlarmConfiguration configuration = Optional.ofNullable(source).orElse(new AlarmConfiguration(new MutableAlarmConfiguration()));
        return new AlarmConfigurationForm(configuration.getNotificationPolicy(), configuration.getRecipients(), configuration.getEmails());

    }

}