package com.veadan.folib.components.email;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import com.veadan.folib.configuration.MutableSmtpConfiguration;
import com.veadan.folib.enums.SMTPConnectionTypeEnum;
import com.veadan.folib.services.ConfigurationManagementService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import javax.inject.Inject;
import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import java.io.File;
import java.util.Properties;

/**
 * @author leipenghui
 * @date 2022/10/17
 **/
@Slf4j
@Component("sendMail")
public class SendMail {

    @Inject
    private ConfigurationManagementService configurationManagementService;

    public void checkMail(MailRequest mailRequest) {
        Assert.notNull(mailRequest, "邮件请求不能为空");
        Assert.notNull(mailRequest.getSendTo(), "邮件收件人不能为空");
        Assert.notNull(mailRequest.getSubject(), "邮件主题不能为空");
        Assert.notNull(mailRequest.getText(), "邮件收件人不能为空");
    }

    /**
     * 发送简单邮件
     *
     * @param mailRequest 邮件信息
     */
    public void sendSimpleMail(MailRequest mailRequest) {
        String sendMailer = mutableSmtpConfiguration().getUsername();
        JavaMailSender javaMailSender = mailSenderConfig();
        SimpleMailMessage message = new SimpleMailMessage();
        checkMail(mailRequest);
        //邮件发件人
        message.setFrom(sendMailer);
        //邮件收件人 1或多个
        message.setTo(mailRequest.getSendTo().split(","));
        //邮件主题
        message.setSubject(mailRequest.getSubject());
        //邮件内容
        message.setText(mailRequest.getText());
        //邮件发送时间
        message.setSentDate(DateUtil.date());
        javaMailSender.send(message);
        log.info("发送邮件成功:{}->{}", sendMailer, mailRequest.getSendTo());
    }

    /**
     * 发送html邮件 可带附件
     *
     * @param mailRequest 邮件信息
     */
    public void sendHtmlMail(MailRequest mailRequest) {
        String sendMailer = mutableSmtpConfiguration().getUsername();
        JavaMailSender javaMailSender = mailSenderConfig();
        MimeMessage message = javaMailSender.createMimeMessage();
        checkMail(mailRequest);
        try {
            MimeMessageHelper helper = new MimeMessageHelper(message, true);
            //邮件发件人
            helper.setFrom(sendMailer);
            //邮件收件人 1或多个
            helper.setTo(mailRequest.getSendTo().split(","));
            //邮件主题
            helper.setSubject(mailRequest.getSubject());
            //邮件内容
            helper.setText(mailRequest.getText(), true);
            //邮件发送时间
            helper.setSentDate(DateUtil.date());
            String filePath = mailRequest.getFilePath();
            if (StringUtils.hasText(filePath)) {
                FileSystemResource file = new FileSystemResource(FileUtil.file(filePath));
                String fileName = filePath.substring(filePath.lastIndexOf(File.separator) + 1);
                helper.addAttachment(fileName, file);
            }
            javaMailSender.send(message);
            log.info("发送邮件成功:{}->{}", sendMailer, mailRequest.getSendTo());
        } catch (MessagingException e) {
            log.error("发送邮件时发生异常！", e);
        }
    }

    private MutableSmtpConfiguration mutableSmtpConfiguration() {
        return configurationManagementService.getMutableConfigurationClone().getSmtpConfiguration();
    }

    /**
     * 邮件配置
     *
     * @return mimeMessage
     */
    public JavaMailSender mailSenderConfig() {
        MutableSmtpConfiguration mutableSmtpConfiguration = mutableSmtpConfiguration();
        String host = mutableSmtpConfiguration.getHost();
        int port = mutableSmtpConfiguration.getPort();
        String username = mutableSmtpConfiguration.getUsername();
        String password = mutableSmtpConfiguration.getPassword();
        String connection = mutableSmtpConfiguration.getConnection();
        JavaMailSenderImpl jms = new JavaMailSenderImpl();
        jms.setHost(host);
        jms.setPort(port);
        jms.setUsername(username);
        jms.setPassword(password);
        jms.setDefaultEncoding("Utf-8");
        Properties p = new Properties();
        if (org.apache.commons.lang3.StringUtils.isNotBlank(username) && org.apache.commons.lang3.StringUtils.isNotBlank(password)) {
            //设置是否需要认证，如果为true,那么用户名和密码就必须的，如果设置false，可以不设置用户名和密码，当然也得看你的对接的平台是否支持无密码进行访问的。
            p.setProperty("mail.smtp.auth", "true");
        } else {
            p.setProperty("mail.smtp.auth", "false");
        }
        //测试连接
        p.setProperty("mail.test-connection", "true");
        //启用调试
        p.setProperty("mail.debug", "true");
        if (SMTPConnectionTypeEnum.PLAIN.getConnection().equals(connection)) {
            //纯文本通信协议
        } else if (SMTPConnectionTypeEnum.SSL.getConnection().equals(connection)) {
            //SSL通信协议
            p.setProperty("mail.smtp.ssl.enable", "true");
            p.setProperty("mail.smtp.ssl.required", "true");
            p.setProperty("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
            p.setProperty("mail.smtp.socketFactory.port", port + "");
            p.setProperty("mail.smtp.socketFactory.fallback", "false");
        } else if (SMTPConnectionTypeEnum.TLS.getConnection().equals(connection)) {
            //TLS通信协议
            p.setProperty("mail.smtp.starttls.enable", "true");
            p.setProperty("mail.smtp.starttls.required", "true");
        }
        jms.setJavaMailProperties(p);
        return jms;
    }
}
