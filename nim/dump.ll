; ModuleID = 'assembly_src/dump.c'
source_filename = "assembly_src/dump.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@stdout = external local_unnamed_addr global ptr, align 8

; Function Attrs: nofree nounwind uwtable
define dso_local void @dump(i64 noundef %0) local_unnamed_addr #0 {
  %2 = alloca [32 x i8], align 16
  call void @llvm.lifetime.start.p0(i64 32, ptr nonnull %2) #3
  %3 = getelementptr inbounds [32 x i8], ptr %2, i64 0, i64 31
  store i8 10, ptr %3, align 1, !tbaa !5
  br label %4

4:                                                ; preds = %4, %1
  %5 = phi i64 [ %0, %1 ], [ %13, %4 ]
  %6 = phi i64 [ 1, %1 ], [ %12, %4 ]
  %7 = urem i64 %5, 10
  %8 = trunc i64 %7 to i8
  %9 = or i8 %8, 48
  %10 = sub i64 31, %6
  %11 = getelementptr inbounds [32 x i8], ptr %2, i64 0, i64 %10
  store i8 %9, ptr %11, align 1, !tbaa !5
  %12 = add i64 %6, 1
  %13 = udiv i64 %5, 10
  %14 = icmp ult i64 %5, 10
  br i1 %14, label %15, label %4, !llvm.loop !8

15:                                               ; preds = %4
  %16 = getelementptr inbounds [32 x i8], ptr %2, i64 0, i64 %10
  %17 = load ptr, ptr @stdout, align 8, !tbaa !10
  %18 = call i64 @fwrite(ptr noundef nonnull %16, i64 noundef 1, i64 noundef %12, ptr noundef %17)
  call void @llvm.lifetime.end.p0(i64 32, ptr nonnull %2) #3
  ret void
}

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0(i64 immarg, ptr nocapture) #1

; Function Attrs: nofree nounwind
declare noundef i64 @fwrite(ptr nocapture noundef, i64 noundef, i64 noundef, ptr nocapture noundef) local_unnamed_addr #2

; Function Attrs: argmemonly mustprogress nocallback nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0(i64 immarg, ptr nocapture) #1

attributes #0 = { nofree nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { argmemonly mustprogress nocallback nofree nosync nounwind willreturn }
attributes #2 = { nofree nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{!"clang version 15.0.0 (git@github.com:llvm/llvm-project.git 64421e191bb23d67a2cd2777dbae9bd171d79482)"}
!5 = !{!6, !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = distinct !{!8, !9}
!9 = !{!"llvm.loop.mustprogress"}
!10 = !{!11, !11, i64 0}
!11 = !{!"any pointer", !6, i64 0}
