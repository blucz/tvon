#include <stdio.h>

#include <CoreFoundation/CoreFoundation.h>
#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>

@interface App : NSObject < NSSpeechRecognizerDelegate > {
    NSSpeechRecognizer *listen;
}

- (id)init;
- (void)dealloc;

@end

@implementation App

-(id)init {
    listen = [[NSSpeechRecognizer alloc] init];

    NSArray *cmds = [NSArray arrayWithObjects:@"t v on",@"tv on", nil];
    [listen setCommands:cmds];
    [listen setDelegate:self];
    [listen setListensInForegroundOnly:NO];
    [listen startListening];
    [listen setBlocksOtherRecognizers:YES];

    return self;
}

- (void)speechRecognizer:(NSSpeechRecognizer *)sender didRecognizeCommand:(id)aCmd {
    printf("tv on indeed\n");
    system("./play samsung_tv_power.wav");
}

- (void)dealloc {
    [listen dealloc];
    [super dealloc];
}

@end

int main() {
    NSAutoreleasePool *pool;
    pool = [[NSAutoreleasePool alloc] init];

    [[App alloc] init];

    printf("hi\n");

    CFRunLoopRun();

    [pool drain];
    return  0;
}
