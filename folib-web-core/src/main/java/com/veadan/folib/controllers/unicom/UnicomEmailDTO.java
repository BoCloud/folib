package com.veadan.folib.controllers.unicom;

import lombok.Data;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-12-03 11:02
 */
@Data
public class UnicomEmailDTO {
    private String title;             // 标题
    private String account;           // 收件人账户，逗号隔开多个
    private String callerIp;          // 调用者IP
    private String cc;                // 抄送
    private String content;           // 正文内容
    private List<FileAttachment> fileList; // 附件文件列表
    private int sendType=1;             // 通知类型：1 简单邮件，2 附件邮件，3 HTML格式邮件，4 短信，5 钉钉
    private String source="folib制品库";            // 邮件来源

    @Data
    public static class FileAttachment {
        private String name;          // 附件文件名
        private String base64;        // 附件文件 base64 格式
        private List<Byte> base64Byte; // 附件文件 base64 数组
        private String createTime;    // 附件创建时间
    }
}
