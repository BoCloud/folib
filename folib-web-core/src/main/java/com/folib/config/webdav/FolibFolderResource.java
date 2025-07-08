package com.folib.config.webdav;

import com.folib.domain.DirectoryListing;
import com.folib.domain.FileContent;
import com.folib.utils.PathUtils;
import io.milton.http.Auth;
import io.milton.http.Range;
import io.milton.http.Request;
import io.milton.http.exceptions.BadRequestException;
import io.milton.http.exceptions.ConflictException;
import io.milton.http.exceptions.NotAuthorizedException;
import io.milton.http.exceptions.NotFoundException;
import io.milton.resource.CollectionResource;
import io.milton.resource.FolderResource;
import io.milton.resource.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @since 2025-03-09 16:12
 */
@Slf4j
public class FolibFolderResource implements FolderResource {

    private final String path;
    private final FileStorageService fileStorageService;

    public FolibFolderResource(String path, FileStorageService fileStorageService) {
        this.path = path;
        this.fileStorageService = fileStorageService;
    }

    protected String getPath(){
        return this.path;
    }

    @Override
    public Resource child(String childName) throws NotAuthorizedException, BadRequestException {
        log.debug("Finding child: {} in path: {}", childName, path);
        String childPath = path + (path.isEmpty() || path.endsWith("/") ? "" : "/") + childName;
        if (!fileStorageService.exists(childPath)) {
            log.debug("Child not found: {}", childPath);
            return null; // 子资源不存在，返回 null
        }

        if (fileStorageService.isDirectory(childPath)) {
            return new FolibFolderResource(childPath, fileStorageService);
        } else {
            return new FolibFileResource(childPath, fileStorageService);
        }
    }

    @Override
    public List<? extends Resource> getChildren() throws NotAuthorizedException, BadRequestException {
        try{
            DirectoryListing directoryListing = this.fileStorageService.listDirectory(path);
            return getResources(directoryListing);
        }catch (Exception e){
            log.error(e.getMessage(), e);
            return new LinkedList<>();
        }
    }

    @Override
    public String getUniqueId() {
        return this.path;
    }

    @Override
    public String getName() {
        return PathUtils.getLastPathElement(path);
    }

    @Override
    public Object authenticate(String user, String password) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth != null && auth.isAuthenticated()) {
            return auth; // 返回认证对象给 Milton
        }
        return null; // 未认证;
    }

    @Override
    public boolean authorise(Request request, Request.Method method, Auth auth) {
        return true;
    }

    @Override
    public String getRealm() {
        return "folib";
    }

    @Override
    public Date getModifiedDate() {
        return fileStorageService.getModifiedDate(this.path);
    }

    @Override
    public String checkRedirect(Request request) throws NotAuthorizedException, BadRequestException {
        return "";
    }

    @Override
    public Date getCreateDate() {
        return fileStorageService.getModifiedDate(this.path);
    }

    @Override
    public void copyTo(CollectionResource collectionResource, String name) throws NotAuthorizedException, BadRequestException, ConflictException {
        log.info("Copying {} to {} with name {}", path, collectionResource.getName(), name);
        String destPath = collectionResource instanceof FolibFolderResource ?
                ((FolibFolderResource) collectionResource).getPath() + (collectionResource.getName().isEmpty() ? "" : "/") + name : name;
        if (fileStorageService.exists(destPath)) {
            return;
        }
        fileStorageService.copyFile(path, destPath);
    }

    @Override
    public void delete() throws NotAuthorizedException, ConflictException, BadRequestException {
        try {
            this.fileStorageService.deleteFile(this.path);
        } catch (IOException e) {
            throw new BadRequestException(e.getLocalizedMessage());
        }
    }

    @Override
    public void sendContent(OutputStream outputStream, Range range, Map<String, String> map, String s) throws IOException, NotAuthorizedException, BadRequestException, NotFoundException {

    }

    @Override
    public Long getMaxAgeSeconds(Auth auth) {
        return 0L;
    }

    @Override
    public String getContentType(String s) {
        return "application/octet-stream";
    }

    @Override
    public Long getContentLength() {
        return null;
    }

    @Override
    public CollectionResource createCollection(String newName) throws NotAuthorizedException, ConflictException, BadRequestException {
        log.debug("Creating collection: {} in path: {}", newName, path);
        if (newName == null || newName.trim().isEmpty() || newName.contains("/")) {
            throw new BadRequestException("Invalid directory name: " + newName);
        }
        String newPath = path + (path.isEmpty() || path.endsWith("/") ? "" : "/") + newName;
        if (fileStorageService.exists(newPath)) {
            throw new ConflictException("Directory or file already exists: " + newPath);
        }
        try {
            fileStorageService.createDirectory(newPath);
        } catch (IOException e) {
            throw new ConflictException("create directory failed: " + newPath);
        }
        return new FolibFolderResource(newPath, fileStorageService);
    }

    @Override
    public void moveTo(CollectionResource collectionResource, String name) throws ConflictException, NotAuthorizedException, BadRequestException {
        log.info("move {} to {} with name {}", path, collectionResource.getName(), name);
        String destPath = collectionResource instanceof FolibFolderResource ?
                ((FolibFolderResource) collectionResource).getPath() + (collectionResource.getName().isEmpty() ? "" : "/") + name : name;
        if (fileStorageService.exists(destPath)) {
            return;
        }
        fileStorageService.moveFile(path, destPath);
    }

    @Override
    public Resource createNew(String newName, InputStream inputStream, Long length, String contentType) throws IOException, ConflictException, NotAuthorizedException, BadRequestException {
        log.debug("Creating new file: {} in path: {}, contentType: {}", newName, path, contentType);
        if (newName == null || newName.trim().isEmpty() || newName.contains("/")) {
            throw new BadRequestException("Invalid file name: " + newName);
        }

        String newPath = path + (path.isEmpty() || path.endsWith("/") ? "" : "/") + newName;
        if (fileStorageService.exists(newPath)) {
            throw new ConflictException("File or directory already exists: " + newPath);
        }
        fileStorageService.saveFileContentFromStream(newPath, inputStream, length);
        return new FolibFileResource(newPath, fileStorageService);
    }

    private List<Resource> getResources(DirectoryListing directoryListing){
        List<Resource> resources = new ArrayList<>();
        for (FileContent file : directoryListing.getFiles()) {
            String childPath = path.isEmpty() ? file.getName() : path + "/" + file.getName();
            FolibFileResource fileResource = new FolibFileResource(childPath, fileStorageService);
            resources.add(fileResource);
        }
        for (FileContent directory : directoryListing.getDirectories()) {
            String childPath = path.isEmpty() ? directory.getName() : path + "/" + directory.getName();
            FolibFolderResource folderResource = new FolibFolderResource(childPath, fileStorageService);
            resources.add(folderResource);
        }
        return resources;
    }
}
