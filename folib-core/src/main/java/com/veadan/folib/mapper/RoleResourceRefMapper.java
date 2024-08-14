package com.veadan.folib.mapper;
import java.util.List;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.dto.PermissionsDTO;
import com.veadan.folib.dto.RoleDTO;
import com.veadan.folib.dto.UserRoleDTO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import com.veadan.folib.entity.RoleResourceRef;

 /**
 * 权限表;(role_resource_ref)表数据库访问层
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
@Mapper
public interface RoleResourceRefMapper extends CommonMapper<RoleResourceRef> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    RoleResourceRef queryById(String id);
    /** 
     * 分页查询指定行数据
     *
     * @param roleResourceRef 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    List<RoleResourceRef> queryAllByLimit(RoleResourceRef roleResourceRef, @Param("pageable") Pageable pageable);
    /** 
     * 统计总行数
     *
     * @param roleResourceRef 查询条件
     * @return 总行数
     */
    long count(RoleResourceRef roleResourceRef);
    /** 
     * 新增数据
     *
     * @param roleResourceRef 实例对象
     * @return 影响行数
     */
    int insert(RoleResourceRef roleResourceRef);
    /** 
     * 批量新增数据
     *
     * @param entities List<RoleResourceRef> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<RoleResourceRef> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<RoleResourceRef> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<RoleResourceRef> entities);
    /** 
     * 更新数据
     *
     * @param roleResourceRef 实例对象
     * @return 影响行数
     */
    int update(RoleResourceRef roleResourceRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(String id);

     List<RoleResourceRef> queryAllByRoleId(@Param("roleIds") List<String> roleIds);
     /**
      * 通过主键批量删除数据
      *
      * @param refIds 主键
      * @return 影响行数
      */
     int deleteByRefIds(@Param("refIds") List<String> refIds);

     List<UserRoleDTO> queryRolesByUserName(@Param("username") String userName,@Param("pageable") PageRequest pageRequest);

     RoleDTO getUserByRoleId(@Param("roleId") String roleId);

     /**
      *
      * @param roleId
      * @param username
      * @param storageId
      * @param repositoryId
      * @param resourceEmpty true值则查询资源为空的权限（admin角色直接和用户关联，没有关联资源）
      * @return
      */
     List<PermissionsDTO> queryPermissions(@Param("roleId") String roleId, @Param("username") String username,
                                           @Param("storageId") String storageId, @Param("repositoryId")String repositoryId, @Param("resourceEmpty") boolean resourceEmpty);

     List<RoleResourceRef> queryRoleByUserId(@Param("userId")String userId,@Param("roleIds") List<String> roleIds);

     /**
      * 通过角色id查询api权限
      * @param roleIds
      * @return
      */
     List<RoleResourceRef> queryApiAuthorities(@Param("roleIds") List<String> roleIds);

     List<RoleResourceRef> queryByRoleIds(@Param("roleIds") List<String> roleIds);
 }