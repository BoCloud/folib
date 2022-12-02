package com.veadan.folib.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.IOException;

/**
 * 调用createrepo 命令
 */
@Component
@Slf4j
public class RepodataUtil {

    /**
     * 创建本地rpm  仓的元数据信息
     *
     * @param fileAbsolutePath 生成 repo元数据的全路径
     */
    public void createRepo(String fileAbsolutePath) throws IOException, InterruptedException {
            Runtime runtime = Runtime.getRuntime();
            String command = "createrepo " + fileAbsolutePath;
            Process pro = runtime.exec(command);
            int status = pro.waitFor();
            if (status != 0) {
                log.error("Failed to call shell's command ");
            }
    }

    /**
     * 删除本地rpm 仓的元数据文件夹
     *
     * @param repodataPath 生成 repo元数据的全路径
     */
    public void deleteRepo(String repodataPath) {
        File file = new File(repodataPath);
        if(file.exists()&&file.isDirectory()){
            file.delete();
        }
    }

}
