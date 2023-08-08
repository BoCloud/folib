package com.veadan.folib.controllers.layout.cocoapods;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/***
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 14:58
 * @since x.x.x
 */
@RestController
@RequestMapping("/api/cocoapods/index")
public class CocoapodsIndexController
        extends BaseController {

    //    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity repoArtIndex(@RepositoryMapping Repository repository, HttpServletResponse response) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".specs/");
        final Path repositoryPathTarget = repositoryPath.getTarget();
        
        ByteArrayOutputStream byteArrayOutputStream = null;
        ZipOutputStream zipOutputStream = null;
        try 
        {
            byteArrayOutputStream = new ByteArrayOutputStream();
            zipOutputStream = new ZipOutputStream(byteArrayOutputStream);
            final String indexFolder = repositoryPathTarget.toUri().getPath();
            zipFolder(indexFolder, indexFolder, zipOutputStream);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        finally {
            try {
                byteArrayOutputStream.close();
                zipOutputStream.close();
            } catch (Exception e) {
                logger.error("关闭zip文件流失败", e);
            }
        }

        try {
            response.setHeader("Content-Disposition", "attachment;filename=file.zip");
            response.setContentType("application/octet-stream");
            ServletOutputStream servletOutputStream = response.getOutputStream();
            servletOutputStream.write(byteArrayOutputStream.toByteArray());
        } catch (IOException e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        
        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    private void zipFolder(String rootPath, String srcFolder, ZipOutputStream zip) throws Exception 
    {
        final File folder = new File(srcFolder);
        //遍历文件夹下所有的文件和文件夹
        final File[] files = folder.listFiles();
        
        if (null != files)
        {
            for (File file : files) 
            {
                //如果是文件夹,递归压缩
                if (file.isDirectory()) {
                    zipFolder(rootPath, file.getAbsolutePath(), zip);
                    continue;
                }
    
                //打开文件输入流
                final FileInputStream fi = new FileInputStream(file);
    
                //设置ZIP条目,并打包文件
                final ZipEntry zipEntry = new ZipEntry(file.getAbsolutePath().replace(rootPath, ""));
                zip.putNextEntry(zipEntry);
                System.out.println(zipEntry.getName());
    
                final byte[] bytes = new byte[1024];
                int length;
                while((length = fi.read(bytes)) >= 0) {
                    zip.write(bytes, 0, length);
                }
                fi.close();
            }
        }
    }
}
