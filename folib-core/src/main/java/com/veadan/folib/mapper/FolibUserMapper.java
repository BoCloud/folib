package com.veadan.folib.mapper;

import java.util.List;
import java.util.Set;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.dto.RepositoryPrivilegeDTO;
import com.veadan.folib.dto.UserDTO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Pageable;
import com.veadan.folib.entity.FolibUser;

 /**
 * 用户信息;(folib_user)表数据库访问层
 * @author : fengmaogen
 * @date : 2024-7-9
 */
@Mapper
public interface FolibUserMapper extends CommonMapper<FolibUser> {
     /**
      * 分页查询指定行数据
      *
      * @param folibUser 查询条件
      * @param pageable 分页对象
      * @return 对象列表
      */
     List<UserDTO> queryAllUserRoleByLimit(@Param("user") FolibUser folibUser,  @Param("pageable") Pageable pageable);
    /**
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    FolibUser queryById(Long id);
    /** 
     * 分页查询指定行数据
     *
     * @param folibUser 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    List<FolibUser> queryAllByLimit(@Param("user") FolibUser folibUser, @Param("pageable") Pageable pageable);
    /** 
     * 统计总行数
     *
     * @param folibUser 查询条件
     * @return 总行数
     */
    long count(FolibUser folibUser);
    /** 
     * 新增数据
     *
     * @param folibUser 实例对象
     * @return 影响行数
     */
    int insert(FolibUser folibUser);
    /** 
     * 批量新增数据
     *
     * @param entities List<FolibUser> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<FolibUser> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<FolibUser> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<FolibUser> entities);
    /** 
     * 更新数据
     *
     * @param folibUser 实例对象
     * @return 影响行数
     */
    int update(FolibUser folibUser);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(String id);

     @Deprecated
     List<UserDTO> queryUserRoleByRepositoryAndPrivilege(@Param("repositoryPrivilegeDTOS") List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS);

     List<UserDTO> queryUsersNameResource(@Param("usernames") List<String> usernames, @Param("storageId") String storageId,
                                          @Param("repositoryId") String repositoryId, @Param("path") String path, @Param("refType") String refType);
 }