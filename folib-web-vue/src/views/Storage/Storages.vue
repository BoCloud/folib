<!-- 
	This is the Settings page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>

  <div id="settings">
    <a-row type="flex" :gutter="[24,24]">

      <a-col :span="24" :lg="6">
        <!-- Page Anchors -->
        <a-affix :offset-top=" navbarFixed ? 100 : 10 ">
          <a-card :bordered="false" class="header-solid mb-24">
            <template #title>
              <a-row type="flex" align="middle">
                <a-col :span="24" :md="12" class="col-info">
                  <h6 class="font-semibold m-0">存储列表</h6>
                </a-col>
                <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                  <a class="text-center text-muted font-bold">
                    <h3 v-if="$store.state.user.roles.indexOf('ADMIN')>-1" class="font-semibold text-muted mb-0"
                        @click="createHandleView">+</h3>
                  </a>
                </a-col>
              </a-row>
            </template>
            <a-anchor :targetOffset=" navbarFixed ? 100 : 10 " :affix="false">
              <a-anchor-link v-for="(item,index) in storageData" :key="index" href="javascript:void(null)"
                             :class="{ slectActive: item.id === currentStorage.id }">
                <div slot="title" class="ant-list-item-meta" @click="setCurrentStorage(item)">
                  <a-icon :type="item.basedir===null?'appstore':'cloud'" theme="filled" class="text-gray-6 text-lg"/>
                  <h4 class="ant-list-item-meta-title">
                    <span class="font-regular">{{ item.id }}</span>
                  </h4>
                </div>
              </a-anchor-link>
            </a-anchor>
          </a-card>
        </a-affix>
        <!-- / Page Anchors -->

      </a-col>
      <a-col :span="24" :lg="18">
        <!-- User Profile card -->
        <a-card :bordered="false" id="profile" class="card-profile-head" :bodyStyle="{padding: 0,}">
          <template #title>
            <a-row type="flex" align="middle">
              <a-col :span="24" :md="12" class="col-info">
                <a-avatar :size="74" shape="square" src="images/folib/storage.svg"/>
                <div class="avatar-info">
                  <h4 class="font-semibold m-0">
                    <span>{{ currentStorage.id }}</span>
                    <a-tooltip placement="topLeft">
                      <template slot="title">
                        <span>S3存储</span>
                      </template>
                      <a-icon style="margin-left: 15px" v-if="currentStorage.basedir!=null" type="cloud" theme="filled"
                              class="text-gray-6 text-lg"/>
                    </a-tooltip>
                  </h4>
                  <p>{{ baseUrl }}api/browse/{{ currentStorage.id }} <a>
                    <a-tooltip placement="topLeft">
                      <template slot="title">
                        <span>复制存储空间路径</span>
                      </template>
                      <a-icon type="copy" @click="copy(baseUrl+'api/browse/'+currentStorage.id)"/>
                    </a-tooltip>
                  </a></p>
                </div>
              </a-col>
              <a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
                <a-tooltip placement="topLeft">
                  <template slot="title">
                    <span>修改存储空间</span>
                  </template>
                  <div v-if="$store.state.user.roles.indexOf('ADMIN')>-1" @click="updateHandleView">
                    <svg width="20px" height="20px" viewBox="0 0 40 40" version="1.1" xmlns="http://www.w3.org/2000/svg"
                         xmlns:xlink="http://www.w3.org/1999/xlink">
                      <title>settings</title>
                      <g stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                        <g transform="translate(-2020.000000, -442.000000)" class="fill-dark" fill="#FFFFFF"
                           fill-rule="nonzero">
                          <g transform="translate(1716.000000, 291.000000)">
                            <g transform="translate(304.000000, 151.000000)">
                              <polygon class="color-background" opacity="0.596981957"
                                       points="18.0883333 15.7316667 11.1783333 8.82166667 13.3333333 6.66666667 6.66666667 0 0 6.66666667 6.66666667 13.3333333 8.82166667 11.1783333 15.315 17.6716667"></polygon>
                              <path class="color-background"
                                    d="M31.5666667,23.2333333 C31.0516667,23.2933333 30.53,23.3333333 30,23.3333333 C29.4916667,23.3333333 28.9866667,23.3033333 28.48,23.245 L22.4116667,30.7433333 L29.9416667,38.2733333 C32.2433333,40.575 35.9733333,40.575 38.275,38.2733333 L38.275,38.2733333 C40.5766667,35.9716667 40.5766667,32.2416667 38.275,29.94 L31.5666667,23.2333333 Z"
                                    opacity="0.596981957"></path>
                              <path class="color-background"
                                    d="M33.785,11.285 L28.715,6.215 L34.0616667,0.868333333 C32.82,0.315 31.4483333,0 30,0 C24.4766667,0 20,4.47666667 20,10 C20,10.99 20.1483333,11.9433333 20.4166667,12.8466667 L2.435,27.3966667 C0.95,28.7083333 0.0633333333,30.595 0.00333333333,32.5733333 C-0.0583333333,34.5533333 0.71,36.4916667 2.11,37.89 C3.47,39.2516667 5.27833333,40 7.20166667,40 C9.26666667,40 11.2366667,39.1133333 12.6033333,37.565 L27.1533333,19.5833333 C28.0566667,19.8516667 29.01,20 30,20 C35.5233333,20 40,15.5233333 40,10 C40,8.55166667 39.685,7.18 39.1316667,5.93666667 L33.785,11.285 Z"></path>
                            </g>
                          </g>
                        </g>
                      </g>
                    </svg>
                  </div>
                </a-tooltip>
              </a-col>
            </a-row>
          </template>
        </a-card>

        <a-row type="flex" :gutter="24">
          <a-col :span="8" class="mb-24" v-for="(item,index) in repositories" :key="index">
            <!-- Project Card -->
            <CardProjectFolib
                :title=item.id
                :logo="'images/folib/'+getLayoutType(item)+'.svg'"
                :team="['images/folib/'+item.type+'.svg']"
                :participants="item.type"
                :due="item.policy"
                @handleMenuClick="handleMenuClick"
                @goToDetial="goToDetial(item)"
            >
              <a-tooltip>
                <template slot="title">
                  {{ baseUrl }}api/browse/{{ currentStorage.id }}/{{ item.id }}
                </template>
                <p>http://..../{{ item.id }} <a>
                  <a-icon type="copy" @click="copy(baseUrl+'api/browse/'+currentStorage.id+'/'+item.id)"/>
                </a></p>
              </a-tooltip>
            </CardProjectFolib>
            <!-- / Project Card -->
          </a-col>

          <a-col :span="8" class="mb-24">
            <a-card @click="folibVisibleShow()" class="crm-bar-line header-solid h-full xinjian"
                    :bodyStyle="{padding: 0, height: '100%', display: 'flex', alignItems: 'center', justifyContent: 'center'}">
              <a class="text-center text-muted font-bold">
                <h3 class="font-semibold text-muted mb-0">+</h3>
                <h5 class="font-semibold text-muted">新 建</h5>
              </a>
            </a-card>

          </a-col>
        </a-row>

      </a-col>
    </a-row>
    <a-modal v-model="showsTorageFormModal" :footer="null" :forceRender="true" title="新建存储空间"
             on-ok="showsTorageFormModal = false" width="50%">
      <a-form
          :form="storageCreateData"
          :hideRequiredMark="true"
          @submit.prevent="handleCreateSubmit"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" label="存储空间名称" :colon="false">
              <a-input v-model="storageCreateData.id"
                       placeholder="存储空间名称">
                <a-icon slot="prefix" type="appstore"/>
              </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" label="存储类型" :colon="false">
              <a-radio-group name="radioGroup" default-value="local" @change="changeStorageType()"
                             v-model="storageCreateData.type">
                <a-radio value="local">
                  本地存储
                </a-radio>
                <a-radio value="S3">
                  S3存储
                </a-radio>
              </a-radio-group>
            </a-form-item>
            <p>说明:</p>
            <ul class="pl-15 text-muted">
              <li>默认为本地存储即:NFS本地目录存储</li>
              <li>S3存储：默认以存储空间名称作为桶名,您也可以自定义桶名称</li>
              <li><strong>注意：存储空间名称、存储类型、S3存储桶路径，一旦创建不可修改</strong></li>
            </ul>
            <a-form-item v-if="storageCreateData.type==='S3'" class="tags-field mb-10" label="S3路径" :colon="false">
              <a-card :bordered="false" class="bg-gray-3 shadow-0 mb-24" :bodyStyle="{padding: '8px'}">
                <a-row type="flex" align="middle">
                  <a-col>
                    <strong
                        class="font-semibold">{{
                        storageCreateData.bucket ? '/' + storageCreateData.bucket : null
                      }}/{{ storageCreateData.id }}</strong>
                  </a-col>
                  <a-col class="ml-auto">
                    <a-input v-if="storageCreateData.isNotCustom" v-model="storageCreateData.bucket"
                             placeholder="桶名称" class="font-regular text-sm text-dark" style="width: 150px;">
                      <a-icon slot="prefix" type="cloud"/>
                    </a-input>
                    <a-button v-if="!storageCreateData.isNotCustom"
                              @click="() => (storageCreateData.isNotCustom = true)" size="small" type="link"
                              class="ml-10 px-25 font-bold">自定义
                    </a-button>
                    <a-button v-if="storageCreateData.isNotCustom"
                              @click="() => (storageCreateData.isNotCustom = false, delete storageCreateData.bucket)"
                              size="small" type="link" class="ml-10 px-25 font-bold">取消自定义
                    </a-button>
                  </a-col>
                </a-row>
              </a-card>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="userInfo.roles.indexOf('ADMIN')>-1" label="管理员选择"
                         :colon="false">
              <a-select v-model="storageCreateData.admin"
                        style="width: 100%"
                        model="default"
                        show-search
                        placeholder="请选择管理员">
                <a-select-option v-for="(tag,index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
                </a-select-option>
              </a-select>
            </a-form-item>
            <a-form-item class="tags-field mb-10"
                         v-if="userInfo.roles.indexOf('ADMIN')>-1 || userInfo.name===currentStorage.admin"
                         label="用户成员选择" :colon="false">
              <a-select v-model="storageCreateData.users" mode="tags" :defaultValue="storageCreateData.users"
                        style="width: 100%"
                        placeholder="例如：*">
                <a-select-option v-for="(tag,index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
                </a-select-option>
              </a-select>
            </a-form-item>
            <a-form-item class="mb-10" :colon="false">

            </a-form-item>
          </a-col>
          <a-col :span="12">
            <!--            <a-button key="back" @click="deleteCurrentTask" class="px-30" size="small" type="danger">Delete</a-button>-->
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button key="submit" class="px-30" size="small" type="primary" htmlType="submit">创建</a-button>
            <a-button key="back" @click="showsTorageFormModal = false" class="px-30 ml-10" size="small">取消</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>

    <a-modal v-model="showStorageUpdate" :footer="null" :forceRender="true" title="修改或删除存储空间"
             on-ok="showStorageUpdate = false" width="50%">
      <a-form
          :hideRequiredMark="true"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <a-form-item class="tags-field mb-10" label="存储空间名称" :colon="false">
              <a-input disabled v-model="currentStorage.id"
                       placeholder="存储空间名称">
                <a-icon slot="prefix" type="appstore"/>
              </a-input>
            </a-form-item>
            <a-form-item class="tags-field mb-10" label="存储类型" :colon="false">
              <a-radio-group disabled name="radioGroup" default-value="local" @change="changeStorageUpdateType()"
                             v-model="currentStorage.type">
                <a-radio value="local">
                  本地存储
                </a-radio>
                <a-radio value="S3">
                  S3存储
                </a-radio>
              </a-radio-group>
            </a-form-item>
            <p>Tip:</p>
            <ul class="pl-15 text-muted">
              <li>存储空间名称不允许修改</li>
              <li>存储类型、S3类型的桶均不允许修改</li>
            </ul>
            <a-form-item v-if="currentStorage.type==='S3'" class="tags-field mb-10" label="S3路径" :colon="false">
              <a-card :bordered="false" class="bg-gray-3 shadow-0 mb-24" :bodyStyle="{padding: '8px'}">
                <a-row type="flex" align="middle">
                  <a-col>
                    <strong
                        class="font-semibold">{{
                        currentStorage.bucket ? '/' + currentStorage.bucket : null
                      }}/{{ currentStorage.id }}</strong>
                  </a-col>
                  <a-col class="ml-auto">
                    <a-input v-if="currentStorage.isNotCustom" v-model="currentStorage.bucket"
                             placeholder="桶名称" class="font-regular text-sm text-dark" style="width: 150px;">
                      <a-icon slot="prefix" type="cloud"/>
                    </a-input>
                    <a-button disabled v-if="!currentStorage.isNotCustom"
                              @click="() => (currentStorage.isNotCustom = true)" size="small" type="link"
                              class="ml-10 px-25 font-bold">自定义
                    </a-button>
                    <a-button v-if="currentStorage.isNotCustom"
                              @click="() => (currentStorage.isNotCustom = false,  currentStorage.bucket=null)"
                              size="small" type="link" class="ml-10 px-25 font-bold">取消自定义
                    </a-button>
                  </a-col>
                </a-row>
              </a-card>
            </a-form-item>
            <a-form-item class="tags-field mb-10" v-if="userInfo.roles.indexOf('ADMIN')>-1" label="管理员选择"
                         :colon="false">
              <a-select v-model="currentStorage.admin"
                        style="width: 100%"
                        model="default"
                        show-search
                        placeholder="请选择管理员">
                <a-select-option v-for="(tag,index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
                </a-select-option>
              </a-select>
            </a-form-item>
            <a-form-item class="tags-field mb-10"
                         v-if="userInfo.roles.indexOf('ADMIN')>-1 || userInfo.name===currentStorage.admin"
                         label="用户成员选择" :colon="false">
              <a-select v-model="currentStorage.users" mode="tags" :defaultValue="currentStorage.users"
                        style="width: 100%"
                        placeholder="例如：*">
                <a-select-option v-for="(tag,index) in userList" :key="index" :value="tag.username">
                  {{ tag.username }}
                </a-select-option>
              </a-select>
            </a-form-item>
            <a-form-item class="mb-10" :colon="false">

            </a-form-item>
          </a-col>
          <a-col :span="12">

          </a-col>
        </a-row>
        <p>说明（请谨慎操作！！！）:</p>
        <ul class="pl-15 text-muted">
          <li>你选择的管理员/成员列表将拥有该存储空间的使用权限</li>
          <li>删除:只删除存储配置，每日0点会定时清理</li>
          <li>若强制删除则无法恢复仓库列表</li>
        </ul>
        <a-row :span="24">
          <a-col :span="12" class="text-left">
            <a-button @click="handleUpdateSubmit" class="px-30" size="small" type="primary" htmlType="submit">修改
            </a-button>
            <a-button @click="showStorageUpdate = false" class="px-30 ml-10" size="small">取消</a-button>
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button @click="storageDelHandle" class="px-30 ml-10" type="danger" size="small">删除</a-button>
            <a-button @click="storageForceDelHandle" class="px-30 ml-10" type="dashed" size="small">强制删除</a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <a-modal v-model="deleteFormVisible" :footer="null" :forceRender="true" on-back="deleteFormVisible = false">
      <a-form
          :form="delForm"
          :hideRequiredMark="true"
      >
        <a-row :gutter="[24]">
          <a-col :span="24">
            <h6 class="text-center font-regular">你确定要删除<a>{{ willDelId }}</a>这个制品仓库么？请谨慎操作</h6>
          </a-col>
          <a-col :span="8">
          </a-col>
          <a-col :span="8">
            <a-form-item class="mb-10" :colon="false">
              <a-input
                  v-decorator="[ 'id', ]"
                  placeholder="仓库名称">
              </a-input>
            </a-form-item>
          </a-col>
          <a-col :span="8">
            <!--            <a-button key="back" @click="deleteCurrentTask" class="px-30" size="small" type="danger">Delete</a-button>-->
          </a-col>
        </a-row>
        <p>说明（请谨慎操作！！！）:</p>
        <ul class="pl-15 text-muted">
          <li>删除:只逻辑删除，不删除安装包</li>
          <li>若强制删除则无法恢复仓库列表</li>
        </ul>
        <a-row :span="24">
          <a-col :span="12" class="text-left">
            <a-button key="back" @click="deleteFormVisible = false" class="px-30 ml-10" size="small">取消</a-button>
          </a-col>
          <a-col :span="12" class="text-right">
            <a-button @click="delRepositoryResponseEntity" class="px-30 ml-10" type="danger" size="small">删除</a-button>
            <a-button @click="delRepositoryResponseEntityForce" class="px-30 ml-10" type="dashed" size="small">强制删除
            </a-button>
          </a-col>
        </a-row>
      </a-form>
    </a-modal>
    <a-drawer placement="right" width="65%" title="新建制品库" :visible="folibVisible" @close="closeUserDialog">
      <div class="mx-auto m-50" style="max-width: 1000px;">

        <!-- Header -->
        <h3 class="mt-25 mb-5 text-center">开始{{folibRepositoryEditDisabled?'修改':'新建'}}你的制品库</h3>
        <h5 class="text-center font-regular">将会在<a>{{ currentStorage.id }}</a>存储空间下{{folibRepositoryEditDisabled?'修改':'新建'}}制品仓库</h5>
        <div class="my-50" style="max-width: 1000px;">

          <!-- Steps -->
          <a-steps progress-dot v-model="step">
            <a-step
                v-if="folibRepository.type==='hosted'||folibRepository.type==='proxy'||folibRepository.type==='group'"
                title="类型选择"/>
            <a-step
                v-if="folibRepository.type==='hosted'||folibRepository.type==='proxy'||folibRepository.type==='group'"
                title="基础信息"/>
            <a-step v-if="folibRepository.type==='proxy'" title="远程配置"/>
            <a-step v-if="folibRepository.type==='group'" title="组合配置"/>
          </a-steps>
          <!-- / Steps -->

        </div>
        <!-- / Header -->

        <!-- Wizard form cards -->
        <div class="mb-50">
          <!-- Step 1 : About -->
          <a-card
              v-if="step === 0&&(folibRepository.type==='hosted'||folibRepository.type==='proxy'||folibRepository.type==='group')"
              :bordered="false" class="header-solid">

            <h5 class="font-regular text-center">{{folibRepositoryEditDisabled?'不可修改，请点击下一步':'不知道怎么选择?'}} </h5>
            <p class="text-center">{{folibRepositoryEditDisabled?'修改模式下不可以更换仓库类型！':'根据图标以及下发的类型名称进行识别，找到你要选择的仓库类型吧！'}}</p>

            <a-form
                :form="form"
                class="mt-30"
                :hideRequiredMark="true"
            >
              <a-row type="flex" :gutter="[24]">
                <a-col :span="24" :md="20" :lg="18" class="mx-auto">
                  <a-row class="checkbox-group" type="flex" :gutter="[50]">
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='raw' ? 'active' : '']"
                           @click="toggleCheckbox('raw')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/raw.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Raw</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='maven' ? 'active' : '']"
                           @click="toggleCheckbox('maven')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/maven.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Maven</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='npm' ? 'active' : '']"
                           @click="toggleCheckbox('npm')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/npm.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>npm</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='yarn' ? 'active' : '']"
                           @click="toggleCheckbox('yarn')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/yarn.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>yarn</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='nuget' ? 'active' : '']"
                           @click="toggleCheckbox('nuget')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/nuget.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>NuGet</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='ivy' ? 'active' : '']"
                           @click="toggleCheckbox('ivy')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/ivy.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Ivy</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='sbt' ? 'active' : '']"
                           @click="toggleCheckbox('sbt')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/sbt.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>sbt</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='gradle' ? 'active' : '']"
                           @click="toggleCheckbox('gradle')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/gradle.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Gradle</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='rpm' ? 'active' : '']"
                           @click="toggleCheckbox('rpm')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/rpm.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>rpm</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='docker' ? 'active' : '']"
                           @click="toggleCheckbox('docker')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/docker.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Docker</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='php' ? 'active' : '']"
                           @click="toggleCheckbox('php')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/php.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>php</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='pypi' ? 'active' : '']"
                           @click="toggleCheckbox('pypi')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/pypi.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>PyPi</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='conan' ? 'active' : '']"
                           @click="toggleCheckbox('conan')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/conan.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Conan</h6>
                    </a-col>
                    <a-col :span="4">
                      <div class="checkbox-label"
                           :class="[layoutChecked==='helm' ? 'active' : '']"
                           @click="toggleCheckbox('helm')"
                      >
                        <a-avatar
                            :size="44"
                            shape="square"
                            style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
                        >
                          <img src="images/folib/helm.svg" style="width: 100%;" alt="">
                        </a-avatar>
                      </div>
                      <h6>Helm</h6>
                    </a-col>
                  </a-row>
                  <a-checkbox-group class="d-none" v-model="checkedList" :options="checkboxOptions"/>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <!--                  <a-button @click="moveStep(-1)" class="px-25">PREV</a-button>-->
                </a-col>
                <a-col :span="12" class="text-right">
                  <a-button :disabled="!layoutChecked" type="primary" @click="moveStep(1)" class="px-25">下一步
                  </a-button>
                </a-col>
              </a-row>
            </a-form>
          </a-card>

          <!-- Step 2 : Account -->
          <a-card
              v-else-if="step === 1&&(folibRepository.type==='hosted'||folibRepository.type==='proxy'||folibRepository.type==='group')"
              :bordered="false" class="header-solid">
            <h5 class="font-regular text-center">OK,接下来要填写基础信息</h5>
            <p class="text-center">
              {{ layoutChecked === 'docker' ? '你选择的是Docker仓库类型' : '选择不同的仓库策略要配置的流程不太一样' }}</p>
            <a-form
                :form="form"
                :hideRequiredMark="true"
            >
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-item class="mb-10" label="仓库名称" :colon="false">
                    <a-input :disabled="folibRepositoryEditDisabled"  placeholder="不要出现仓库类型的关键字" v-model="folibRepositoryIds"
                             :addon-after="'-'+layoutChecked"/>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="策略" :colon="false">
                    <a-select  :disabled="layoutChecked==='docker'||folibRepositoryEditDisabled" default-value="hosted"
                              v-model="folibRepository.type">
                      <a-select-option value="hosted">
                        本地
                      </a-select-option>
                      <a-select-option value="proxy">
                        代理
                      </a-select-option>
                      <a-select-option value="group">
                        组合
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="版本策略" :colon="false">
                    <a-select default-value="release" v-model="folibRepository.policy">
                      <a-select-option value="release">
                        release
                      </a-select-option>
                      <a-select-option value="snapshot">
                        snapshot
                      </a-select-option>
                      <a-select-option value="mixed">
                        mixed
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item class="mb-10" label="仓库路径" :colon="false">
                    <a-input disabled placeholder="当前存储为分布式，不支持存储路径定义"
                             v-model="folibRepository.basedir"/>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="制品大小限制(MB)" :colon="false">
                    <a-input v-model:value="artifactMaxSize" addon-after="MB">
                    </a-input>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="服务状态" :colon="false">
                    <a-select default-value="In Service" v-model="folibRepository.status">
                      <a-select-option value="In Service">
                        开放服务
                      </a-select-option>
                      <a-select-option value="Out of Service">
                        关闭服务
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="4">
                  <a-form-item class="mb-10" label="回收站" :colon="false">
                    <a-checkbox v-model="folibRepository.trashEnabled">
                      {{ folibRepository.trashEnabled ? '开启' : '关闭' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="删除" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDelete">
                      {{ folibRepository.allowsDelete ? '允许' : '不允许' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="强制删除" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsForceDeletion">
                      {{ folibRepository.allowsForceDeletion ? '允许' : '不允许' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="上传部署" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDeployment">
                      {{ folibRepository.allowsDeployment ? '允许' : '不允许' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="上传覆盖" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsRedeployment">
                      {{ folibRepository.allowsRedeployment ? '允许' : '不允许' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="目录浏览" :colon="false">
                    <a-checkbox v-model="folibRepository.allowsDirectoryBrowsing">
                      {{ folibRepository.allowsDirectoryBrowsing ? '允许' : '不允许' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-button @click="moveStep(-1)" class="px-25">回退</a-button>
                </a-col>
                <a-col :span="12" class="text-right">
                  <a-button v-if="folibRepository.type==='hosted'" type="primary" @click="addOrUpdateRepositoryHandel()"
                            class="px-25">完成
                  </a-button>
                  <a-button v-else-if="folibRepository.type!=='hosted'" type="primary" @click="moveStep(1)"
                            class="px-25">下一步
                  </a-button>
                </a-col>
              </a-row>
            </a-form>
          </a-card>

          <!-- Step 3 : Address -->
          <a-card v-else-if="step === 2&&(folibRepository.type==='proxy')" :bordered="false" class="header-solid">
            <h5 class="font-regular text-center">远程仓库配置</h5>
            <p class="text-center">
              您选择的是远程仓库类型，还需要配置一下你的远程库地址，也可以开启本地代理访问远程地址</p>
            <a-form
                :form="form"
                :hideRequiredMark="true"
            >
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-form-item class="mb-10" label="远程访问地址" :colon="false">
                    <a-input placeholder="http://xxxx或者https://xxxx" v-model="folibRepository.remoteRepository.url"/>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="用户名" :colon="false">
                    <a-input v-model="folibRepository.remoteRepository.username"
                             autocomplete="new-text"
                             placeholder="远程仓库访问用户名"/>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item class="mb-10" label="密码" :colon="false">
                    <a-input-password v-model="folibRepository.remoteRepository.password"
                                      autocomplete="new-password"
                                      placeholder="远程仓库访问密码"/>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="定时检查时间(秒)" :colon="false">
                    <a-input placeholder="默认60秒" v-model="folibRepository.remoteRepository.checkIntervalSeconds"
                             addon-after="s"/>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="检查机制" :colon="false">
                    <a-select default-value="None" v-model="folibRepository.remoteRepository.checksumPolicy">
                      <a-select-option value="None">
                        无
                      </a-select-option>
                      <a-select-option value="Strict">
                        严格
                      </a-select-option>
                      <a-select-option value="Log">
                        日志记录
                      </a-select-option>
                      <a-select-option value="Warn">
                        高级
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="本地代理" :colon="false">
                    <a-checkbox v-model="enableHostProxy" @change="proxyConfigurationHandle">
                      {{ enableHostProxy ? '开启' : '不开启' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" label="目录浏览" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.allowsDirectoryBrowsing">
                      {{ folibRepository.remoteRepository.allowsDirectoryBrowsing ? '运行' : '不允许' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" label="自动封锁" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.autoBlocking">
                      {{ folibRepository.remoteRepository.autoBlocking ? '开启' : '关闭' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" label="校验和 检查" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.checksumValidation">
                      {{ folibRepository.remoteRepository.checksumValidation ? '开启' : '关闭' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
                <a-col :span="3">
                  <a-form-item class="mb-10" label="远程索引下载" :colon="false">
                    <a-checkbox v-model="folibRepository.remoteRepository.downloadRemoteIndexes">
                      {{ folibRepository.remoteRepository.downloadRemoteIndexes ? '下载' : '不下载' }}
                    </a-checkbox>
                  </a-form-item>
                </a-col>
              </a-row>

              <a-row v-if="enableHostProxy" :gutter="[24]">
                <a-col :span="24">
                  <h5 class="font-regular text-center">代理设置</h5>
                  <p class="text-center">只有在当前制品库无法访问远程代理库的情况下，可以使用该功能</p>
                </a-col>
                <a-col :span="8">
                  <a-form-item class="mb-10" label="ProxyHost" :colon="false">
                    <a-input placeholder="ProxyHost" v-model="folibRepository.proxyConfiguration.host"/>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="端口号" :colon="false">
                    <a-input v-model:value="folibRepository.proxyConfiguration.port" placeholder="端口号"/>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="协议" :colon="false">
                    <a-select default-value="None" v-model="folibRepository.proxyConfiguration.type">
                      <a-select-option value="">
                        None
                      </a-select-option>
                      <a-select-option value="HTTP">
                        HTTP
                      </a-select-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="用户名" :colon="false">
                    <a-input v-model:value="folibRepository.proxyConfiguration.username"
                             placeholder="proxy的用户名，没有可以不填写"/>
                  </a-form-item>
                </a-col>
                <a-col :span="4">
                  <a-form-item class="mb-10" label="密码" :colon="false">
                    <a-input-password v-model:value="folibRepository.proxyConfiguration.password"
                                      placeholder="远程仓库访问密码"/>
                  </a-form-item>
                </a-col>

              </a-row>
              <a-row :gutter="[24]">
                <a-col :span="12">
                  <a-button @click="moveStep(-1)" class="px-25">回退</a-button>
                </a-col>
                <a-col :span="12" class="text-right">
                  <a-button type="primary" @click="addOrUpdateRepositoryHandel()" class="px-25">完成</a-button>
                </a-col>
              </a-row>
            </a-form>
          </a-card>

          <a-card v-else-if="step === 2&&(folibRepository.type==='group')" :bordered="false" class="header-solid">
            <h5 class="font-regular text-center">组合仓库配置</h5>
            <p class="text-center">你选择的是组合仓库，可以将多个仓库从左至右进行拖动，进行组合.</p>
            <div class="kanban-page mb-24">
              <div id="kanban" class="kanban">
                <draggable :list="boards" :animation="200" class="kanban-boards" ghost-class="ghost-card"
                           group="boards">
                  <FolibKanbanBoard
                      v-for="(board) in boards"
                      :key="board.id"
                      :board="board"
                  >
                    <draggable :list="board.tasks" :animation="200" ghost-class="ghost-card" group="tasks">

                      <FolibKanbanTask
                          v-for="(task) in board.tasks"
                          :key="task.id"
                          :task="task"
                          :boardId="board.id"
                      ></FolibKanbanTask>

                    </draggable>

                  </FolibKanbanBoard>
                  <!-- / Kanban Board -->

                </draggable>
              </div>
            </div>
            <a-row :gutter="[24]">
              <a-col :span="12">
                <a-button @click="moveStep(-1)" class="px-25">回退</a-button>
              </a-col>
              <a-col :span="12" class="text-right">
                <a-button type="primary" @click="addOrUpdateRepositoryHandel()" class="px-25">完成</a-button>
              </a-col>
            </a-row>
          </a-card>

        </div>
        <!-- / Wizard form cards -->

      </div>
    </a-drawer>
  </div>

</template>

<script>
import {
  getStorages,
  updateStorages,
  getLibrary,
  getLibraryByQuery,
  addOrUpdateRepository,
  getRepositoryResponseEntity,
  delRepositoryResponseEntity,
  getBaseUrl, createStorages, deleteStorages
} from "@/api/folib"
import {getUsers} from "@/api/users";
import CardProjectFolib from "@/components/Cards/CardProjectFolib"
import {getLayoutType, genLayoutType, groupRepositoriesBuild, objectToGroupRepositories} from "@/utils/layoutUtil"
import draggable from "vuedraggable";
import FolibKanbanBoard from "@/components/Kanban/FolibKanbanBoard";
import FolibKanbanTask from "@/components/Kanban/FolibKanbanTask";
import storage from 'store';
import store from '@/store';
import {checkMachineCode} from "@/api/settings";

export default {
  components: {
    CardProjectFolib,
    draggable,
    FolibKanbanBoard,
    FolibKanbanTask,
  },
  props: ['navbarFixed'],
  data() {
    return {
      userInfo: {},
      showStorageUpdate: false,
      userList: [],
      baseUrl: null,
      folibVisible: false,
      storageData: [],
      currentStorage: {
        id: null,
        basedir: null,
        admin: undefined,
        users: [],
        isNotCustom: false,
        type: 'local',
        bucket: null,
      },
      currentDefultStorage: {
        id: null,
        basedir: null,
        admin: undefined,
        users: [],
        isNotCustom: false,
        type: 'local',
        bucket: null,
      },
      showsTorageFormModal: false,
      delForm: this.$form.createForm(this, {name: "del"}),
      storageCreateData: {
        id: null,
        basedir: null,
        admin: undefined,
        isNotCustom: false,
        type: 'local',
        bucket: null,
        users: []
      },
      storageCreateDefultData: {
        id: null,
        basedir: null,
        admin: undefined,
        isNotCustom: false,
        type: 'local',
        bucket: null,
        users: []
      },
      storageForm: this.$form.createForm(this, {name: "storage"}),
      visibility: true,
      slack: true,
      spotify: true,
      atlassian: true,
      asana: false,
      tags: ['Vuejs', 'Angular', 'React'],
      repositories: [],
      queryData: {
        term: null,
        storageId: null,
        withStorageId: true,
        type: null,
        layout: null
      },
      //抽屉相关
      layoutChecked: null,
      enableHostProxy: false,
      step: 0,
      // Checkbox'es array of checked options.
      checkedList: ['Design'],
      // Checkbox'es array of all options.
      checkboxOptions: ['Design', 'Code', 'Develop'],
      willDelId: null,
      deleteFormVisible: false,
      // Step's form object
      form: this.$form.createForm(this, {name: 'steps'}),
      folibRepositoryIds: "",
      artifactMaxSize: 100,
      folibRepositoryEditDisabled: false,
      folibRepository: {
        allowsDelete: true,
        allowsDeployment: true,
        allowsDirectoryBrowsing: false,
        allowsForceDeletion: false,
        allowsRedeployment: false,
        artifactCoordinateValidators: null,
        artifactMaxSize: 0,
        basedir: null,
        checksumHeadersEnabled: true,
        groupRepositories: [],
        httpConnectionPool: null,
        id: "",
        layout: "",
        policy: "release",
        proxyConfiguration: {
          host: "",
          nonProxyHosts: [],
          password: null,
          port: null,
          type: null,
          username: null
        },
        remoteRepository: {
          allowsDirectoryBrowsing: true,
          autoBlocking: true,
          autoImportRemoteSSLCertificate: false,
          checkIntervalSeconds: 60,
          checksumPolicy: 'None',
          checksumValidation: true,
          downloadRemoteIndexes: true,
          password: "",
          url: "",
          username: ""
        },
        repositoryConfiguration: null,
        secured: false,
        status: "In Service",
        storageProvider: "local",
        trashEnabled: true,
        type: "hosted",
      },
      folibRepositoryBack: {
        allowsDelete: true,
        allowsDeployment: true,
        allowsDirectoryBrowsing: false,
        allowsForceDeletion: false,
        allowsRedeployment: false,
        artifactCoordinateValidators: null,
        artifactMaxSize: 0,
        basedir: null,
        checksumHeadersEnabled: true,
        groupRepositories: [],
        httpConnectionPool: null,
        id: "",
        layout: "",
        policy: "release",
        proxyConfiguration: null,
        remoteRepository: {
          allowsDirectoryBrowsing: true,
          autoBlocking: true,
          autoImportRemoteSSLCertificate: false,
          checkIntervalSeconds: 60,
          checksumPolicy: 'None',
          checksumValidation: true,
          downloadRemoteIndexes: true,
          password: "",
          url: "",
          username: ""
        },
        repositoryConfiguration: null,
        secured: false,
        status: "In Service",
        storageProvider: "local",
        trashEnabled: true,
        type: "hosted",
      },
      boards: [
        {
          id: "folibHub",
          title: "可选择制品仓库",
          tasks: []
        },
        {
          id: "folibGoup",
          title: "已组合仓库",
          tasks: []
        }
      ],

    };
  },
  created() {
    this.userInfo = store.state.user
    this.getStorages();
    this.getBaseUrl();

    const params = storage.get('libView_repository')

    if (params) {
      this.currentStorage.id = params.item.storageId
    }


    if (!this.currentStorage.id) {
      this.currentStorage.id = this.storageData[0].id
      this.currentStorage.basedir=this.storageData[0].basedir
    }

    this.getLibrary(this.currentStorage)


  },
  computed: {},
  methods: {
    changeStorageType() {
      if (this.storageCreateData.type === 'S3') {
        this.storageCreateData.isNotCustom = false
      } else {
        delete this.storageCreateData.bucket
      }
    },
    changeStorageUpdateType() {
      if (this.currentStorage.type === 'S3') {
        this.currentStorage.isNotCustom = false
      } else {
        delete this.currentStorage.bucket
      }
    },
    createHandleView() {

      this.showsTorageFormModal = true
      this.getUsersList()
    },
    updateHandleView() {

      this.showStorageUpdate = true
      this.getUsersList()
    },
    proxyConfigurationHandle() {
      if (this.enableHostProxy) {
        if (!this.folibRepository.proxyConfiguration) {
          this.folibRepository.proxyConfiguration = {
            host: "",
            nonProxyHosts: [],
            password: null,
            port: null,
            type: null,
            username: null
          }
        }
      } else {
        this.folibRepository.proxyConfiguration = null
      }
    },
    copy(url) {
      var input = document.createElement("input"); // 创建input对象
      input.value = url; // 设置复制内容
      document.body.appendChild(input); // 添加临时实例
      input.select(); // 选择实例内容
      document.execCommand("Copy"); // 执行复制
      document.body.removeChild(input); // 删除临时实例
      // console.log(url)
      setTimeout(() => {
        this.$notification.success({
          message: '复制成功'
        })
      }, 100)
    },
    // 初始化获取集成url
    getBaseUrl() {
      getBaseUrl().then(res => {
        this.baseUrl = res
      })
    },
    filterOption(input, option) {
      return (
          option.componentOptions.children[0].text.toLowerCase().indexOf(input.toLowerCase()) >= 0
      );
    },
    storageDelHandle(e) {
      if (this.currentStorage.id != null) {
        this.deleteStoragesKeyBuff()
        deleteStorages(this.currentStorage, false).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showStorageUpdate = false;
          this.getStorages();
        })
        this.currentStorage=this.currentDefultStorage
      }
    },
    deleteStoragesKeyBuff() {
      if (this.currentStorage.id != null) {
        if (this.currentStorage.type === 'S3') {
          this.currentStorage.basedir = this.currentStorage.bucket ? '/' + this.currentStorage.bucket + '/' + this.currentStorage.id : '/' + this.currentStorage.id
        } else {
          this.currentStorage.basedir = null
        }
        delete this.currentStorage.bucket
        delete this.currentStorage.isNotCustom
        delete this.currentStorage.type
      }
    },
    storageForceDelHandle() {
      if (this.currentStorage.id != null) {
        this.deleteStoragesKeyBuff()
        deleteStorages(this.currentStorage, true).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showStorageUpdate = false;
          this.getStorages();
        })
        this.currentStorage=this.currentDefultStorage
      }
    },
    handleCreateSubmit(e) {
      if (this.storageCreateData.id != null) {
        if (this.storageCreateData.type === 'S3') {
          this.storageCreateData.basedir = this.storageCreateData.bucket ? '/' + this.storageCreateData.bucket + '/' + this.storageCreateData.id : '/' + this.storageCreateData.id
        } else {
          this.storageCreateData.basedir = null
        }
        delete this.storageCreateData.bucket
        delete this.storageCreateData.isNotCustom
        delete this.storageCreateData.type

        createStorages(this.storageCreateData).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showsTorageFormModal = false;
          this.getStorages();
        })
        this.storageCreateData=this.storageCreateDefultData

      }

    },
    handleUpdateSubmit(e) {
      if (this.currentStorage.id != null) {
        this.deleteStoragesKeyBuff()

        updateStorages(this.currentStorage).then(response => {
          setTimeout(() => {
            this.$notification.success({
              message: response.message,
            })
          }, 100)
          this.showStorageUpdate = false;
          this.getStorages();
        })
        this.currentStorage=this.currentDefultStorage

      }

    },
    getUsersList() {
      getUsers().then(res => {
        this.userList = res.users
      })
    },
    getStorages() {
      getStorages().then(response => {
        this.storageData = response.storages;
        this.cacheStorage()
      })


    },
    setCurrentStorage(item) {
      if (!item.admin || item.admin === '') {
        item.admin = undefined
      }
      if (!item.users || item.users.length == 0) {
        item.users = []
      }
      this.currentStorage.id = item.id
      this.currentStorage.basedir = item.basedir
      this.currentStorage.admin = item.admin
      this.currentStorage.users = item.users
      if (this.currentStorage.basedir !== null) {
        this.currentStorage.type = 'S3'
        this.currentStorage.isNotCustom = false
        this.currentStorage.bucket = null
      } else {
        this.currentStorage.type = 'local'
        this.currentStorage.isNotCustom = false
        this.currentStorage.bucket = null
      }
      this.getLibrary(item);
    },
    getLibrary(item) {
      getLibrary(item.id).then(response => {
        this.repositories = response.repositories
      })
    },
    cacheStorage() {
      let cache = storage.get("libView_repository");
      if (!cache || !cache.item.id) {
        if (this.storageData) {
          let item = this.storageData[0]
          if (item && item.id) {
            this.setCurrentStorage(item)
            item.storageId = item.id
            storage.set("libView_repository", {item, baseUrl: this.baseUrl})
          }
        }
      }
    if(this.currentStorage.id){

      this.currentStorage.basedir=this.storageData.filter(f=>f.id===this.currentStorage.id)[0].basedir

    }


    },
    getLayoutType(item) {
      // console.log(getLayoutType(item))
      return getLayoutType(item)
    },
    genLayoutType(layout) {
      return genLayoutType(layout)
    },
    closeUserDialog() {
      this.folibVisible = false
      this.folibRepository=this.folibRepositoryBack

    },
    repositoryList() {
      this.queryData.storageId = this.currentStorage.id
      this.queryData.layout = this.genLayoutType(this.layoutChecked)
      getLibraryByQuery(this.queryData).then(res => {
        const tasksObj = objectToGroupRepositories(this.folibRepository.groupRepositories, res, this.folibRepository.id)
        this.boards[0].tasks = tasksObj.enableSelect
        this.boards[1].tasks = tasksObj.isSelect
      })
    },
    folibVisibleShow() {
      checkMachineCode().then(res => {
        if (res.haveError) {
          setTimeout(() => {
            this.$notification.open({
              class: 'ant-notification-warning',
              message: 'License不正确',
              description: '请检查License是否存在',
            });
          }, 1000);
        } else {

          if (res.dalyOut) {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: 'License已过期',
                description: '请续期后再添加制品仓库',
              });
            }, 1000);
          } else {


            if (this.currentStorage.id) {
              this.folibRepositoryEditDisabled=false
              this.layoutChecked = null
              this.step = 0

              this.folibRepositoryIds = ""
              this.folibVisible = true
            } else {
              setTimeout(() => {
                this.$notification.open({
                  class: 'ant-notification-warning',
                  message: '操作不正确',
                  description: '你应该先从左侧选中一个存储空间，然后再新建仓库',
                });
              }, 1000);
            }
          }

        }

      })

    },
    //抽屉=========
    // Move step by a distance.
    moveStep(distance) {
      this.step += distance;
      if (this.step === 2 && this.folibRepository.type === "group") {
        this.repositoryList()
      }
    },

    // Toggle an item from the checkbox.
    toggleCheckbox(item) {
      this.layoutChecked = item
    },
    addOrUpdateRepositoryHandel() {
      this.folibRepository.id = this.folibRepositoryIds + '-' + this.layoutChecked
      //构建basedir
      if(this.currentStorage.basedir){
        this.folibRepository.basedir=this.currentStorage.basedir+'/'+this.folibRepository.id
        this.folibRepository.storageProvider='s3'
      }else {
        this.folibRepository.basedir=null
        this.folibRepository.storageProvider='local'
      }
      //将选中的layout图标转换为接口识别的
      this.folibRepository.layout = genLayoutType(this.layoutChecked)
      //将组合好的仓库转为groupRepository
      if (this.step === 2 && this.folibRepository.type === 'group' && this.boards[1].tasks.length > 0) {
        this.folibRepository.groupRepositories = groupRepositoriesBuild(this.boards[1].tasks)
        this.folibRepository.proxyConfiguration = null
        this.folibRepository.remoteRepository = null
      }
      if (this.step === 2 && this.folibRepository.type === 'proxy') {
        this.folibRepository.groupRepositories = null
        delete this.folibRepository.remoteRepository.customConfiguration
      }
      if (this.step === 1 && this.folibRepository.type === 'hosted') {
        this.folibRepository.groupRepositories = null
        this.folibRepository.proxyConfiguration = null
        this.folibRepository.remoteRepository = null
      }


      delete this.folibRepository.customConfigurations
      delete this.folibRepository.storageId
      this.folibRepository.artifactMaxSize = this.artifactMaxSize * 1024 * 1024
      addOrUpdateRepository(this.currentStorage.id, this.folibRepository.id, this.folibRepository).then(res => {
        if (!res.error) {
          setTimeout(() => {
            this.$notification.open({
              class: 'ant-notification-success',
              message: '仓库配置完成',
              description: res.message,
            });
          }, 1000);
        }
        this.folibRepository = this.folibRepositoryBack
        this.folibVisible = false
        this.getLibrary(this.currentStorage)
        this.step = 0
      })

    },
    getRepositoryResponseEntity(repositoryId) {
      getRepositoryResponseEntity(this.currentStorage.id, repositoryId).then(res => {
        if (res.id === repositoryId) {
          this.folibRepository = res
          this.layoutChecked = getLayoutType(res)
          this.artifactMaxSize = this.folibRepository.artifactMaxSize / (1024 * 1024)
          const a = this.folibRepository.id;
          if (a.search(this.layoutChecked) !== -1) {
            this.folibRepositoryIds = a.substring(0, a.indexOf('-' + this.layoutChecked))
          }

          // console.log(this.layoutChecked)
          this.folibRepositoryEditDisabled=true
          this.folibVisible = true

        }
      })

    },
    delRepositoryResponseEntity() {
      this.delForm.validateFields((err, values) => {
        // console.log(values,this.willDelId)
        if (!err) {
          // console.log(values,this.willDelId)
          if (this.willDelId === values.id) {
            delRepositoryResponseEntity(this.currentStorage.id, values.id, false).then(response => {
              setTimeout(() => {
                this.$notification.open({
                  class: 'ant-notification-success',
                  message: '成功',
                  description: values.id + '已删除',
                });
              }, 100)
            })

            this.deleteFormVisible = false;
            this.getLibrary(this.currentStorage)
          } else {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: '填写错误',
                description: '要删除的内容填写错误',
              });
            }, 1000);
          }

        }
      });
    },
    delRepositoryResponseEntityForce() {
      this.delForm.validateFields((err, values) => {
        // console.log(values,this.willDelId)
        if (!err) {
          // console.log(values,this.willDelId)
          if (this.willDelId === values.id) {
            delRepositoryResponseEntity(this.currentStorage.id, values.id, true).then(response => {
              setTimeout(() => {
                this.$notification.open({
                  class: 'ant-notification-success',
                  message: '成功',
                  description: values.id + '已删除',
                });
              }, 100)
            })

            this.deleteFormVisible = false;
            this.getLibrary(this.currentStorage)
          } else {
            setTimeout(() => {
              this.$notification.open({
                class: 'ant-notification-warning',
                message: '填写错误',
                description: '要删除的内容填写错误',
              });
            }, 1000);
          }

        }
      });
    },
    handleMenuClick(e, title) {
      if (e === "edit" && title !== null) {
        this.getRepositoryResponseEntity(title)
      } else if (e === "delete" && title !== null) {
        this.willDelId = title
        this.deleteFormVisible = true
      }

    },
    goToDetial(item) {
      storage.set("libView_repository", {item, baseUrl: this.baseUrl})
      this.$router.push({
        name: 'libDetial'
      })
    }
  },
};
</script>

<style lang="scss" scoped>
.slectActive {
  background-color: #eeeeee !important;
  border-radius: 8px;
}

.kanban-board {
  min-width: 450px;
  box-shadow: none;
  background: #e9ecef;
  margin-right: 20px;

  > .ant-card-body {
    padding-bottom: 30px;
  }
}

.drawer-folib {
  min-width: 450px;
  box-shadow: none;
  background: #e9ecef;

}

.ghost-card {
  opacity: 0.5;
  background: #F7FAFC;
  border: 1px solid #4299e1;
}

.xinjian {
  min-height: 203px;
}

#settings::v-deep {
  .ant-list {
    width: 100%;
  }

  .ant-list-item-meta-avatar {
    margin-right: 8px;
  }

  .ant-list-item-meta {
    align-items: center;
  }

  .ant-list-item-meta-title {
    margin: 0;
  }

  .ant-anchor-ink::before {
    display: none;
  }

  .ant-anchor-link {
    padding: 0;
    margin-top: 8px;
  }

  .ant-anchor-link a {
    width: 100%;
    border-radius: 8px;
    color: #67748e !important;
    padding: 10px 16px;
    background-color: transparent;
    transition: background-color 0.3s ease-in;
  }

  .ant-anchor-link a:hover {
    background-color: #eeeeee;
    cursor: pointer;
  }

  .ant-anchor-link a svg g {
    fill: #344767;
  }

  .ant-anchor-link a svg {
    margin-right: 8px;
  }

  .card-profile-head {
    margin: 0 0 24px;
  }

  .tags-field .ant-form-item-control {
    line-height: 33px;
  }

  .form-tag.ant-tag {
    border-radius: 20px;
    padding: 4px 10px;
    font-size: 12px;
    font-weight: 500;
    margin-right: 3.75px;
    margin-bottom: 3.75px;
    background-color: #3a416f;
    border: 1px solid #3a416f;
    color: #fff;
  }

  .form-tag.ant-tag .anticon-close {
    color: #fff;
    height: 16px;
    border-left: 1px solid hsla(0, 0%, 100%, .3);
    padding-left: 5px;
    padding-top: 2px;
    opacity: .75;
  }

  .form-tag.ant-tag .anticon-close:hover {
    color: #fff;
    opacity: 1;
  }

  .tags-field .ant-input {
    margin-bottom: 5px;
    margin-top: 4px;
  }

  .tags-field .ant-select {
    .ant-select-selection__choice__remove i {
      color: #fff;
      height: 16px;
      border-left: 1px solid hsla(0, 0%, 100%, .3);
      padding-left: 5px;
      padding-top: 2px;
      opacity: .75;

      &:hover {
        color: #fff;
        opacity: 1;
      }
    }

    .ant-select-selection__rendered > ul > li:not(.ant-select-search) {
      border-radius: 20px;
      padding: 2px 27px 2px 10px;
      font-size: 12px;
      font-weight: 500;
      margin-right: 3.75px;
      margin-bottom: 3.75px;
      background-color: #3a416f;
      border: 1px solid #3a416f;
      color: #fff;
      line-height: 2;
      height: 30px;
    }

    .ant-select-selection--multiple {
      padding: 8px 10px;
    }
  }
}

</style>
