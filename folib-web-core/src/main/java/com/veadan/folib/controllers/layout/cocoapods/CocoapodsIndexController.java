package com.veadan.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.nio.file.Path;

/***
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 14:58
 * @since x.x.x
 */
@RestController
@RequestMapping("/api/cocoapods")
public class CocoapodsIndexController
        extends BaseController {

    //    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @GetMapping(value = "/{storageId}/{repositoryId}/index/fetchIndex")
    public ResponseEntity repoArtIndex(@RepositoryMapping Repository repository, HttpServletRequest request, HttpServletResponse response) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".specs/");
        final Path repositoryPathTarget = repositoryPath.getTarget();

        ServletOutputStream servletOutputStream = null;
        TarArchiveOutputStream tarArchiveOutputStream = null;
        try 
        {
            response.setHeader("Content-Disposition", "attachment;filename=file.tar.gz");
            response.setContentType("application/x-gzip");
            
            final String indexFolder = repositoryPathTarget.toUri().getPath();
            servletOutputStream = response.getOutputStream();
            tarArchiveOutputStream = new TarArchiveOutputStream(new GzipCompressorOutputStream(servletOutputStream));
            tarArchiveOutputStream.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);
            tarGzFolder(indexFolder, indexFolder, tarArchiveOutputStream);
            servletOutputStream.flush();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
        finally {
            try {
                if (null != tarArchiveOutputStream)
                { tarArchiveOutputStream.close(); }
                if (null != servletOutputStream)
                { servletOutputStream.close(); }
            } catch (Exception e) {
                logger.error("关闭zip文件流失败", e);
            }
        }

        return new ResponseEntity<>("ok", HttpStatus.OK);
    }

    private static void tarGzFolder(String rootPath, String srcFolder, TarArchiveOutputStream archiveOutputStream) throws Exception 
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
                    tarGzFolder(rootPath, file.getAbsolutePath(), archiveOutputStream);
                    continue;
                }
    
                if (file.getName().endsWith(".podspec"))
                {
                    final byte[] bytes = FileUtil.readBytes(file);
                    final TarArchiveEntry entry = new TarArchiveEntry(file.getAbsolutePath().replace(rootPath, StringUtils.EMPTY));
                    entry.setSize(bytes.length);
                    archiveOutputStream.putArchiveEntry(entry);
                    archiveOutputStream.write(bytes);
                    archiveOutputStream.closeArchiveEntry();
                }
            }
        }
    }

}
