unit DownLoadThreadIndy;

interface
uses
  Windows, Classes, SysUtils, SyncObjs, idComponent, idhttp, idSocks, IdIOHandlerSocket, _DGL_Point, Dialogs;

//{$define _USE_INDY_10_}


type
  TSpeedClc = class(TObject)
  private
    FCountTimeSpace: integer;
    FTotalSize: integer;
    FAllSize: integer;
    FDataDeque: TPointDeque;
    procedure SetCountTimeSpace(const Value: integer);
    procedure SetTotalSize(const Value: integer);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Inti();
    property CountTimeSpace: integer read FCountTimeSpace write SetCountTimeSpace;
    procedure AddData(incSize: integer);
    function CurSpeed_KB_S(): double;
    property AllSize: integer read FAllSize;
    property TotalSize: integer read FTotalSize write SetTotalSize;
  end;

type

  TOnProgressEvent = procedure(Sender: TObject; TotalSize, Readed: Integer) of object;
  TOnOnDownLoadedEvent = procedure(Sender: TObject; IsDownLoadOk: boolean) of object;

   //我的测试 
  TWSocksInfo = record
    useProxy: boolean; //我的测试
    SocksServer: string; //我的测试
    SocksLevel: integer; //我的测试
    SocksAuthentication: boolean; //我的测试
    SocksUsercode: string; //我的测试
    SocksPassword: string; //我的测试
    SocksPort: string; //我的测试
  end;

   //我的测试 
  TDownLoadThread = class(TThread)
  private
    FReadTimeout: Integer;
    procedure PrivateClearHTTP;
    procedure PrivateCreateHTTP;
  protected
    FDestFileName: string;
    FTempDestFileName: string;
    FUrl: string;
    FHttpCli: TIdHttp; //wsocket http 我的测试
    FStream: TStream; //我的测试
    FOnProgress: TOnProgressEvent; //我的测试
    FBytesReaded: integer; //我的测试
    FFileSize: integer; //我的测试
    HandlerSocket: TIdIOHandlerSocket;
    SocksInfo: TIdSocksInfo;
    FDownLoadOk: boolean;
    FSocksInfo: TWSocksInfo;
    FOnDownLoaded: TOnOnDownLoadedEvent;
    FOnError: TNotifyEvent;
    procedure HTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
{$IFNDEF _USE_INDY_10_}const{$ENDIF}AWorkCountMax: Integer);
    procedure HTTPWork(Sender: TObject; AWorkMode: TWorkMode;
{$IFNDEF _USE_INDY_10_}const{$ENDIF}AWorkCount: Integer);
    procedure HTTPStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure SetOnProgress(const Value: TOnProgressEvent);
    procedure SetOnDownLoaded(const Value: TOnOnDownLoadedEvent);
    procedure SetOnError(const Value: TNotifyEvent);
    procedure DoError();
  protected
    procedure PrivateCreate(url, FileName: string; aSocksInfo: TWSocksInfo;
      is_String: boolean); virtual;
    function GetDownLoadTmpFileName(const SrcURL: string): string;
    function CreateDownLoadTmpFileStream(const SrcURL: string): TStream; virtual;
    function DoDoDownLoadedStreamTest(): boolean; virtual;
  protected
    procedure UpdateProgress; //我的测试
    procedure DoDownLoaded; //
    function getResultString: string;
    procedure FreeStream();
  public
    constructor Create(const SrcURL, DestFileName: string; aSocksInfo: TWSocksInfo); overload;
    constructor Create(const SrcURL: string; aSocksInfo: TWSocksInfo); overload;
    destructor Destroy; override;
    procedure DoDownLoad(); virtual;
    procedure Execute; override;

    property ResultString: string read GetResultString;
    property OnProgress: TOnProgressEvent read FOnProgress write SetOnProgress;
    property OnDownLoaded: TOnOnDownLoadedEvent read FOnDownLoaded write SetOnDownLoaded;
    property OnError: TNotifyEvent read FOnError write SetOnError;

    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
  end;


type
   //我的测试
  TDownLoadThreadEx = class(TDownLoadThread)
  protected
    FSocksInfo: TWSocksInfo;
    FMD5Stream: TStream;
  protected
    procedure PrivateCreate(url, FileName: string; aSocksInfo: TWSocksInfo;
      is_String: boolean); override;
    function GetSafePos(const SrcURL: string): integer;
  protected
    function CreateDownLoadTmpFileStream(const SrcURL: string): TStream; override;
    function DoDoDownLoadedStreamTest(): boolean; override;
  public
    destructor Destroy; override;
  end;



implementation
uses
  md5, UnitMD5Check, FORMS, IdException;



procedure TDownLoadThread.PrivateClearHTTP();
begin
  if FHttpCli <> nil then
    self.FHttpCli.IOHandler := nil;

  //我的测试
  FreeAndNil(self.FHttpCli);

  //我的测试
  FreeAndNil(self.HandlerSocket);
  FreeAndNil(self.SocksInfo);
end;

procedure TDownLoadThread.PrivateCreateHTTP();
const
  AuthMed: array[boolean] of TSocksAuthentication
  = (saNoAuthentication, saUsernamePassword);
  SOCKSVERS: array[4..5] of TSocksVersion
  = (svSocks4, svSocks5);
begin
  PrivateClearHTTP();
  //我的测试
  self.FHttpCli := TIdHttp.Create(nil);

  assert(FReadTimeout>0);
  FHttpCli.ReadTimeout := FReadTimeout; //

  FHttpCli.Request.Accept := '*/*';
  FHttpCli.Request.UserAgent := 'Mozilla/3.0 (compatible; IndyLibrary)';
 //FHttpCli.Request.ContentType := 'application/x-www-form-urlencoded';
  FHttpCli.Request.ContentType := 'application/octet-stream';
  FHttpCli.Request.Connection := 'Keep-Alive';

{$IFDEF _USE_INDY_10_}
//indy 10
  if FSocksInfo.useProxy then
  begin
    FHttpCli.ProxyParams.ProxyServer := FSocksInfo.SocksServer;
    FHttpCli.ProxyParams.ProxyPort := strtoint(FSocksInfo.SocksPort);
    FHttpCli.ProxyParams.ProxyUsername := FSocksInfo.SocksUsercode;
    FHttpCli.ProxyParams.ProxyPassword := FSocksInfo.SocksPassword;
  end;
{$ELSE}
//indy 9
  self.HandlerSocket := TIdIOHandlerSocket.Create(nil);
  self.FHttpCli.IOHandler := self.HandlerSocket;
  //FHttpCli.ProtocolVersion:=pv1_0;

  self.socksInfo := TIdSocksInfo.create(nil);
  self.HandlerSocket.SocksInfo := self.SocksInfo;

  if FSocksInfo.useProxy then
  begin
    self.HandlerSocket.SocksInfo.Version := SOCKSVERS[FSocksInfo.SocksLevel];
    self.HandlerSocket.SocksInfo.Host := FSocksInfo.SocksServer;
    self.HandlerSocket.SocksInfo.Port := strtoint(FSocksInfo.SocksPort);
    self.HandlerSocket.SocksInfo.Authentication := AuthMed[FSocksInfo.SocksAuthentication];
    self.HandlerSocket.SocksInfo.Username := FSocksInfo.SocksUsercode;
    self.HandlerSocket.SocksInfo.password := FSocksInfo.SocksPassword;
  end;
{$ENDIF}

  //我的测试
  self.FHttpCli.OnWorkBegin := self.HTTPWorkBegin;
  self.FHttpCli.OnWork := self.HTTPWork;
  self.FHttpCli.OnStatus := self.HTTPStatus;

end;

procedure TDownLoadThread.PrivateCreate(url: string; FileName: string;
  aSocksInfo: TWSocksInfo; is_String: boolean);
const
  AuthMed: array[boolean] of TSocksAuthentication
  = (saNoAuthentication, saUsernamePassword);
  SOCKSVERS: array[4..5] of TSocksVersion
  = (svSocks4, svSocks5);
begin
  FReadTimeout := 20000;
  self.FSocksInfo := aSocksInfo;
  self.FUrl := url;
  FDestFileName := FileName;
  FTempDestFileName := self.GetDownLoadTmpFileName(url);
  if not is_string then
  begin
    //我的测试
    self.FStream := self.CreateDownLoadTmpFileStream(url);
  end
  else
  begin
    self.FStream := TStringStream.Create('');
  end;

  FreeOnTerminate := True;

  self.PrivateCreateHTTP();

  self.FFileSize := 0;
  self.FBytesReaded := 0;

end;

//我的测试

procedure TDownLoadThread.HTTPWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
{$IFNDEF _USE_INDY_10_}const{$ENDIF}AWorkCountMax: Integer);
var
  tFileSize: integer;
begin
  if aWorkCountMax > 0 then
  begin
    //tFileSize:=AWorkCountMax;//self.FHttpCli.Response.ContentLength;
    //tFileSize:=self.FStream.Size;
    //Assert(aWorkCountMax=strtoint(self.FHttpCli.Response.RawHeaders.Values['Content-Length']));
    //self.FFileSize := tFileSize;
  end;
end;

//我的测试

procedure TDownLoadThread.HTTPWork(Sender: TObject; AWorkMode: TWorkMode;
{$IFNDEF _USE_INDY_10_}const{$ENDIF}AWorkCount: Integer);
begin
  self.FBytesReaded := AWorkCount;

  //我的测试
  self.Synchronize(self.UpdateProgress);
end;



destructor TDownLoadThread.destroy;
begin
  //我的测试

  if FStream <> nil then
    FreeStream();

  PrivateClearHTTP();

  inherited;
end;

procedure TDownLoadThread.UpdateProgress;
begin
  if self.Terminated then
  begin
    self.FHttpCli.Disconnect();
  end;

  //Assert(FStream.Size=FFileSize);
  if assigned(FOnProgress) then
    FOnProgress(Self, FFileSize, FStream.Position);
end;


procedure TDownLoadThread.Execute;
begin
  try
    DoDownLoad();
    self.Synchronize(self.DoDownLoaded);
  except
    self.Synchronize(self.DoError);
  end;
end;

function TDownLoadThread.DoDoDownLoadedStreamTest(): boolean;
begin
  result := true;
end;

procedure TDownLoadThread.DoDownLoad;
  procedure SetDownloadPos();
  begin
    FHttpCli.Response.ContentLength := self.FFileSize;
    if FStream.Position > 0 then
    begin
      FHttpCli.Request.ContentRangeStart := FStream.Position; //我的测试
      FHttpCli.Request.ContentRangeEnd := self.FFileSize;
    end;
  end;

  procedure ReSetDownloadHTTP();
  begin
    self.PrivateCreateHTTP();
    FHttpCli.Head(self.FUrl);
  end;

  procedure tryDownLoad(MaxErrorCount, MaxTimeOutCount: integer; out isTimeOutExit: boolean);
  var
    tryTimeOutCount: integer;
    tryErrorCount: integer;
  begin
    tryTimeOutCount := 0;
    tryErrorCount := 0;
    isTimeOutExit := false;
    while (tryTimeOutCount < MaxTimeOutCount) and (tryErrorCount < MaxErrorCount) and (FStream.Position < FFileSize) do
    begin
      try
        SetDownloadPos();
        self.FHttpCli.Get(self.FUrl, self.FStream);
      except
        on E: EIdReadTimeout do
        begin
          inc(tryTimeOutCount);
          ReSetDownloadHTTP();
        end
      else
        begin
          inc(tryErrorCount);
          if tryErrorCount > MaxErrorCount then
            raise;
          ReSetDownloadHTTP();
        end;
      end;
    end;
    isTimeOutExit := tryTimeOutCount >= MaxTimeOutCount;
  end;
var
  isOldDownOK: boolean;
  isTimeOutExit: boolean;
  curLinkHaveDataLength: integer;
const csMaxErrorCount = 6;
const csMaxTimeOutCount = 3;
begin
  FDownLoadOk := false;
  isOldDownOK := false;
  FHttpCli.Head(self.FUrl);
  FFileSize := FHttpCli.Response.ContentLength;
 // ShowMessage('Line:355--' + Self.FUrl + '--' + IntToStr(FFileSize));
  if (FStream.Position > 0) then
  begin
    if FStream.Position > FFileSize then
    begin
      FStream.Size := FFileSize;
      FStream.Position := FFileSize;
    end;
    isOldDownOK := (FStream.Position = FFileSize);
  end;

  curLinkHaveDataLength := FStream.Position;

  if isOldDownOK then
    FDownLoadOk := true
  else
  begin
    tryDownLoad(csMaxErrorCount, csMaxTimeOutCount, isTimeOutExit);
    FDownLoadOk := (self.FStream.Position = FFileSize);
   //  FDownLoadOk :=(Self.FStream.Position = FHttpCli.Response.ContentLength);
    if (not FDownLoadOk) and (self.FBytesReaded <= curLinkHaveDataLength) and (isTimeOutExit) then
    begin
      FStream.Size := FFileSize;
     // ShowMessage('Line:377--' + Self.FUrl + '--' + IntToStr(FFileSize));
      FStream.Position := 0;
      tryDownLoad(csMaxErrorCount, csMaxTimeOutCount, isTimeOutExit);
      FDownLoadOk := (self.FStream.Position = FFileSize);
    end;
  end;


  if FDownLoadOk then
    FDownLoadOk := DoDoDownLoadedStreamTest();

  if (self.FDestFileName <> '') and (FDownLoadOk) then
  begin
    FreeStream();
    //Assert(FTempDestFileName<>FDestFileName);
    DeleteFile(FDestFileName);
    RenameFile(self.FTempDestFileName, FDestFileName);
  end;
end;

function TDownLoadThread.getResultString: string;
begin
  if Self.FStream is TStringStream then
    result := (self.FStream as TStringStream).dataString
  else
    Assert(false);
end;

procedure TDownLoadThread.SetOnProgress(const Value: TOnProgressEvent);
begin
  FOnProgress := Value;
end;

constructor TDownLoadThread.Create(const SrcURL, DestFileName: string;
  aSocksInfo: TWSocksInfo);
begin
  inherited Create(true);
  self.PrivateCreate(SrcURL, DestFileName, aSocksInfo, false);
end;

constructor TDownLoadThread.Create(const SrcURL: string;
  aSocksInfo: TWSocksInfo);
begin
  inherited Create(true);
  self.PrivateCreate(SrcURL, '', aSocksInfo, true);
end;

procedure TDownLoadThread.SetOnDownLoaded(
  const Value: TOnOnDownLoadedEvent);
begin
  FOnDownLoaded := Value;
end;

procedure TDownLoadThread.DoDownLoaded;
begin
  if FStream is TFileStream then
    FreeStream();
  if Assigned(self.FOnDownLoaded) then self.FOnDownLoaded(self, self.FDownLoadOk);
end;

procedure TDownLoadThread.SetOnError(const Value: TNotifyEvent);
begin
  FOnError := Value;
end;

procedure TDownLoadThread.DoError;
begin
  if FStream is TFileStream then
    FreeStream();
  if Assigned(FOnError) then self.FOnError(self);
end;

procedure TDownLoadThread.FreeStream;
begin
  if (self.FHttpCli <> nil) and (self.FHttpCli.Response <> nil) then
    self.FHttpCli.Response.ContentStream := nil;
  FreeAndNil(FStream);
end;

function TDownLoadThread.GetDownLoadTmpFileName(
  const SrcURL: string): string;
  function GetFileName(aURL: string): string;
  begin
    Result := Copy(aURL, pos('download/', aURL) + 9, Length(aURL));
  end;
begin
  result := extractFilePath(ParamStr(0)) + 'downLoad_' + StrMD5Std(GetFileName(SrcURL)) + '.tmp';
end;

function TDownLoadThread.CreateDownLoadTmpFileStream(
  const SrcURL: string): TStream;
begin
  result := TFileStream.Create(self.FTempDestFileName, fmCreate);
end;

{ TDownLoadThreadEx }

function TDownLoadThreadEx.CreateDownLoadTmpFileStream(
  const SrcURL: string): TStream;
var
  safePos: integer;
begin
  safePos := GetSafePos(SrcURL);
  if (safePos <= 0) then
  begin
    result := inherited CreateDownLoadTmpFileStream(SrcURL);
  end
  else
  begin
    result := TFileStream.Create(self.FTempDestFileName, fmOpenReadWrite);
    result.Position := safePos;
    result.Size := safePos;
  end;
end;


destructor TDownLoadThreadEx.Destroy;
begin
  FreeAndNil(FMD5Stream);
  inherited;
end;

function TDownLoadThreadEx.DoDoDownLoadedStreamTest(): boolean;
const
  csIgnore = '我的测试 ';
var
  ErrorSections: TErrorSectionArray;
  tmpMD5FileRecord: TMD5FileRecord;
  ck: boolean;
begin
  result := inherited DoDoDownLoadedStreamTest();
  if not result then exit;

  Assert(result);

  if self.FMD5Stream.Size = 0 then exit;
  if not UnitMD5Check.MD5FileToMD5FileRecord(FMD5Stream, tmpMD5FileRecord) then exit;

  //data  md5 check
  try
    ck := UnitMD5Check.CheckDataFile(self.FStream, FMD5Stream, ErrorSections);
    //todo
    if (ck) and (length(ErrorSections) = 0) then exit;
  except
    result := false;
    exit;
  end;

  //is ignore我的测试 
  result := (ID_OK = windows.MessageBox(0, csIgnore, 'DownLoad ok', MB_OKCANCEL + MB_ICONWARNING));
end;

function TDownLoadThreadEx.GetSafePos(const SrcURL: string): integer;
var
  MD5FileThead: TDownLoadThread;
  ErrorSections: TErrorSectionArray;
  DataFile: TFileStream;
  DataFileSize: integer;
  i: integer;
  tmpMD5FileRecord: TMD5FileRecord;
begin
  result := 0;
  DataFileSize := 0;
  FMD5Stream.Size := 0;

  try
    MD5FileThead := nil;
    DataFile := nil;
    ErrorSections := nil;
    try
      //DEBUG_IsTestMD5
      {if true then
      begin
        //check
        DataFile :=TFileStream.Create('Test_BEDE94B62E0169C5701E302E8067DD1F.downLoad.tmp',fmOpenRead or fmShareDenyWrite);
        DataFileSize:=DataFile.Size;
        if (not UnitMD5Check.CheckDataFile(
            'Test_BEDE94B62E0169C5701E302E8067DD1F.downLoad.tmp',
            'Test_BEDE94B62E0169C5701E302E8067DD1F.downLoad.tmp.md5',ErrorSections)) then
          exit;
      end
      else }
      begin
        //DownLoad md5 File
        if (LowerCase(ExtractFileExt(SrcURL)) <> '.md5') and (LowerCase(ExtractFileExt(SrcURL)) <> '.vol') then
        begin
          MD5FileThead := TDownLoadThread.Create(SrcURL + '.md5', self.FSocksInfo);
          MD5FileThead.FreeOnTerminate := false;
          MD5FileThead.DoDownLoad();

          if UnitMD5Check.MD5FileToMD5FileRecord(MD5FileThead.FStream, tmpMD5FileRecord) then
          begin
            MD5FileThead.FStream.Position := 0;
            FMD5Stream.CopyFrom(MD5FileThead.FStream, MD5FileThead.FStream.Size);
          end;

          //
          if (not FileExists(FTempDestFileName)) then
            exit;

          //check
          DataFile := TFileStream.Create(FTempDestFileName, fmOpenRead or fmShareDenyWrite);
          DataFileSize := DataFile.Size;
          if (not UnitMD5Check.CheckDataFile(DataFile, MD5FileThead.FStream, ErrorSections)) then
            exit;
        end;
      end;

    finally
      if (DataFile <> nil) then FreeAndNil(DataFile);
      if (MD5FileThead <> nil) then FreeAndNil(MD5FileThead);
    end;
  except
    exit;
  end;
    //

  //get safe pos
  result := DataFileSize;
  for i := 0 to length(ErrorSections) - 1 do
  begin
    if integer(ErrorSections[i].PosIndex) < result then
      result := ErrorSections[i].PosIndex;
  end;
  Assert(result <= DataFileSize);
  Assert(result >= 0);
end;

procedure TDownLoadThreadEx.PrivateCreate(url, FileName: string;
  aSocksInfo: TWSocksInfo; is_String: boolean);
begin
  Assert(FMD5Stream = nil);
  FMD5Stream := TStringStream.Create('');
  FSocksInfo := aSocksInfo;
  inherited;
end;

{ TSpeedClc }

procedure TSpeedClc.AddData(incSize: integer);
var
  NewData: TPoint;
begin
  NewData.x := windows.GetTickCount;
  if (FAllSize = 0) then
    NewData.x := 0;
  NewData.y := incSize;
  self.FDataDeque.PushFront(NewData);
  inc(self.FAllSize, incSize);
end;

function TSpeedClc.CurSpeed_KB_S: double;
var
  newTime: integer;
  oldTime: integer;
  SumSize: integer;
  i: integer;
begin
  newTime := windows.GetTickCount;
  oldTime := newTime - self.FCountTimeSpace;
  SumSize := 0;
  i := 0;
  while (i < self.FDataDeque.Size) do
  begin
    if FDataDeque.Items[i].x >= oldTime then
      inc(SumSize, FDataDeque.Items[i].y)
    else
      break;
    inc(i);
  end;
  result := SumSize * (1.0 / 1024 * 1000) / FCountTimeSpace;
end;

procedure TSpeedClc.SetCountTimeSpace(const Value: integer);
begin
  Assert(Value > 0);
  FCountTimeSpace := Value;
end;


procedure TSpeedClc.Inti;
begin
  self.FTotalSize := 0;
  self.FAllSize := 0;
  self.FDataDeque.Clear;
end;

procedure TSpeedClc.SetTotalSize(const Value: integer);
begin
  FTotalSize := Value;
end;

constructor TSpeedClc.Create();
begin
  inherited;
  FCountTimeSpace := 5000;
  FDataDeque := TPointDeque.Create();
end;

destructor TSpeedClc.Destroy;
begin
  FDataDeque.Free;
  inherited;
end;

procedure TDownLoadThread.HTTPStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  //
end;

end.

