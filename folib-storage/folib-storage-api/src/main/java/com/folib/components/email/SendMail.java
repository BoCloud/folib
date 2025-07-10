/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.components.email;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.SmtpConfiguration;
import com.folib.enums.SMTPConnectionTypeEnum;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.io.FileSystemResource;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import jakarta.inject.Inject;
import java.io.File;
import java.util.Properties;

/**
 * @author veadan
 **/
@Slf4j
@Component("sendMail")
public class SendMail {

    @Inject
    private ConfigurationManager configurationManager;

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
        String sendMailer = getSmtpConfiguration().getUsername();
        if (StringUtils.isBlank(sendMailer)) {
            return;
        }
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
        String sendMailer = getSmtpConfiguration().getUsername();
        if (StringUtils.isBlank(sendMailer)) {
            log.warn("邮件配置信息为空，发送邮件失败");
            return;
        }
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
            if (StringUtils.isNotBlank(filePath)) {
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

    private SmtpConfiguration getSmtpConfiguration() {
        return configurationManager.getConfiguration().getSmtpConfiguration();
    }

    /**
     * 邮件配置
     *
     * @return mimeMessage
     */
    public JavaMailSender mailSenderConfig() {
        SmtpConfiguration smtpConfiguration = getSmtpConfiguration();
        String host = smtpConfiguration.getHost();
        int port = smtpConfiguration.getPort();
        String username = smtpConfiguration.getUsername();
        String password = smtpConfiguration.getPassword();
        String connection = smtpConfiguration.getConnection();
        JavaMailSenderImpl jms = new JavaMailSenderImpl();
        jms.setHost(host);
        jms.setPort(port);
        jms.setUsername(username);
        jms.setPassword(password);
        jms.setDefaultEncoding("Utf-8");
        Properties p = new Properties();
        if (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) {
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
